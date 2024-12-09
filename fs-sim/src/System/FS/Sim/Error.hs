{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | 'HasFS' instance wrapping 'SimFS' that generates errors, suitable for
-- testing error handling.
module System.FS.Sim.Error (
    -- * Simulate Errors monad
    simErrorHasFS
  , simErrorHasFS'
  , runSimErrorFS
  , withErrors
    -- * Streams
  , ErrorStream
  , ErrorStreamGetSome
  , ErrorStreamPutSome
    -- * Generating partial reads/writes
  , Partial (..)
  , partialiseByteCount
  , partialiseWord64
  , partialiseByteString
    -- * Blob
  , Blob (..)
  , blobFromBS
  , blobToBS
    -- * Generating corruption for 'hPutSome'
  , PutCorruption (..)
  , corruptByteString
    -- * Error streams for 'HasFS'
  , Errors (..)
  , allNull
  , emptyErrors
  , genErrors
  , simpleErrors
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad (unless, void)
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Primitive
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.List (intercalate)
import qualified Data.List as List
import           Data.Maybe (catMaybes)
import           Data.Primitive.ByteArray
import           Data.String (IsString (..))
import           Data.Word (Word64)
import           Foreign.C.Types
import           Prelude hiding (null)
import           SafeWildCards
import           System.Posix.Types

import qualified Test.QuickCheck as QC
import           Test.QuickCheck (ASCIIString (..), Arbitrary (..), Gen,
                     suchThat)

import           System.FS.API
import           System.FS.CallStack

import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.MockFS (HandleMock, MockFS)
import qualified System.FS.Sim.STM as Sim
import qualified System.FS.Sim.Stream as Stream
import           System.FS.Sim.Stream (Stream)

{-------------------------------------------------------------------------------
  Streams of errors
-------------------------------------------------------------------------------}

-- | An 'ErrorStream' is a possibly infinite 'Stream' of (@Maybe@)
-- @'FsErrorType'@s.
--
-- 'Nothing' indicates that there is no error.
--
-- Each time the 'ErrorStream' is used (see 'runErrorStream'), the first
-- element ('Nothing' in case the list is empty) is taken from the list and an
-- 'ErrorStream' with the remainder of the list is returned. The first element
-- represents whether an error should be returned or not.
--
-- An 'FsError' consists of a number of fields: 'fsErrorType', a
-- 'fsErrorPath', etc. Only the first fields is interesting. Therefore, we
-- only generate the 'FsErrorType'. The 'FsErrorType' will be used to
-- construct the actual 'FsError'.
type ErrorStream = Stream FsErrorType

-- | 'ErrorStream' for reading bytes from a file: an error or a partial get.
type ErrorStreamGetSome = Stream (Either FsErrorType Partial)

-- | 'ErrorStream' for writing bytes to a file: an error and possibly some
-- corruption, or a partial write.
type ErrorStreamPutSome =
  Stream (Either (FsErrorType, Maybe PutCorruption) Partial)

{-------------------------------------------------------------------------------
  Generating partial reads/writes
-------------------------------------------------------------------------------}

-- | A @'Partial' p@, where @p > 0@, is a number representing how many fewer
-- bytes should be read or written than requested.
newtype Partial = Partial Word64
    deriving (Show)

instance Arbitrary Partial where
  arbitrary = Partial <$> QC.choose (1, 100)
  shrink (Partial p) =
    [Partial p' | p' <- [1..p]]

-- | Given a requested number of bytes to read/write, compute a partial number
-- of bytes to read/write.
--
-- We subtract @p@ from the number of requested bytes. If that would result in 0
-- requested bytes or less, we request 1 byte. If the number of requested bytes
-- was already 0, we can't simulate a partial read so we return 0 again.
partialiseByteCount :: Partial -> ByteCount -> ByteCount
partialiseByteCount (Partial p) c
  | 0 <- c'   = c
  | p >= c'   = 1
  | otherwise = c - fromIntegral p
  where c' = fromIntegral c

-- | Like 'partialiseByteCount', but for 'Word64'.
partialiseWord64 :: Partial -> Word64 -> Word64
partialiseWord64 = coerce partialiseByteCount

-- | Given a bytestring that is requested to be written to disk, use
-- 'partialiseByteCount' to compute a partial bytestring.
partialiseByteString :: Partial -> BS.ByteString -> BS.ByteString
partialiseByteString p bs = BS.take (fromIntegral $ partialiseByteCount p len) bs
  where len = fromIntegral (BS.length bs)

{------------------------------------------------------------------------------
  Blob
------------------------------------------------------------------------------}

-- For the custom 'Show' and 'Arbitrary' instances
--
-- A builder of a non-empty bytestring.
newtype Blob = MkBlob { getBlob :: ByteString }
    deriving (Show)

instance Arbitrary Blob where
    arbitrary = do
      str <- (getASCIIString <$> arbitrary) `suchThat` (not . List.null)
      return $ fromString str
    shrink (MkBlob b) =
      [ fromString s'
      | let s = ASCIIString $ LC8.unpack $ BL.fromStrict b
      , s' <- getASCIIString <$> shrink s
      , not (List.null s') ]

blobToBS :: Blob -> ByteString
blobToBS = getBlob

blobFromBS :: ByteString -> Blob
blobFromBS = MkBlob

instance IsString Blob where
    fromString = blobFromBS . C8.pack

{-------------------------------------------------------------------------------
  Generating corruption for hPutSome
-------------------------------------------------------------------------------}

-- | Model possible corruptions that could happen to a 'hPutSome' call.
data PutCorruption
    = SubstituteWithJunk Blob
      -- ^ The blob to write is substituted with corrupt junk
    | PartialWrite Partial
      -- ^ Only perform the write partially
    deriving (Show)

instance Arbitrary PutCorruption where
  arbitrary = QC.oneof
      [ SubstituteWithJunk <$> arbitrary
      , PartialWrite <$> arbitrary
      ]
  shrink (SubstituteWithJunk blob) =
      [SubstituteWithJunk blob' | blob' <- shrink blob]
  shrink (PartialWrite partial) =
      [PartialWrite partial' | partial' <- shrink partial]

-- | Apply the 'PutCorruption' to the 'BS.ByteString'.
--
-- If the bytestring is substituted by corrupt junk, then the output bytestring
-- __might__ be larger than the input bytestring.
corruptByteString :: BS.ByteString -> PutCorruption -> BS.ByteString
corruptByteString bs pc = case pc of
    SubstituteWithJunk blob -> getBlob blob
    PartialWrite partial    -> partialiseByteString partial bs

-- | Apply the 'PutCorruption' to a 'MutableByteArray'.
--
-- This either means that part of the bytes written to file are subsituted with
-- junk, or that only part of the buffer will be written out to disk due to a
-- partial write.
--
-- With respect to junk substitution, the intent of this function is to model
-- corruption of the bytes written to a file, __not__ corruption of the
-- in-memory buffer itself. As such, we don't corrupt the argument
-- 'MutableByteArray' in place, but instead we return a new 'MutableByteArray'
-- that has the same contents plus some possible corruption. This ensures that
-- the corruption is not visible to other parts of the program that use the same
-- 'MutableByteArray'. Corruption will only be applied to the buffer at the the
-- given 'BufferOffset', up to the requested 'ByteCount'. If there are not
-- enough bytes in the bytearray, then corruption will only apply up until the
-- end of the bytearray.
--
-- With respect to partial writes, the function returns a new number of
-- requested bytes, which is strictly smaller or equal to the input
-- 'ByteCount'.
--
-- NOTE: junk substitution and partial writes are mutually exclusive, and so
-- this functions produces only one effect. Either the buffer contents are
-- changed, or the 'ByteCount' is reduced.
corruptBuffer ::
     PrimMonad m
  => MutableByteArray (PrimState m)
  -> BufferOffset
  -> ByteCount
  -> PutCorruption
  -> m (MutableByteArray (PrimState m), ByteCount)
corruptBuffer buf bufOff c pc = do
    case pc of
      SubstituteWithJunk blob -> do
        len <- getSizeofMutableByteArray buf
        -- this creates an unpinned byte array containing a copy of @buf@. It should
        -- be fine that it is unpinned, because the simulation is fully in-memory.
        copy <- freezeByteArray buf 0 len
        buf' <- unsafeThawByteArray copy
        -- Only corrupt up to the end of the bytearray.
        let lenRemaining = len - unBufferOffset bufOff
        b <- MockFS.intoBuffer buf' bufOff (BS.take lenRemaining (getBlob blob))
        -- Applying the corruption shouldn't have failed because we've ensured
        -- that the bytestring isn't too large to fit into the buffer.
        unless b $ error "corruptBuffer: corruption failed. This probably \
                         \indicates a bug in the fs-sim library."
        pure (buf', c)
      PartialWrite partial ->
        pure (buf, partialiseByteCount partial c)

{-------------------------------------------------------------------------------
  Simulated errors
-------------------------------------------------------------------------------}

-- | Error streams for the methods of the 'HasFS' type class.
--
-- An 'ErrorStream' is provided for each method of the 'HasFS' type class.
-- This 'ErrorStream' will be used to generate potential errors that will be
-- thrown by the corresponding method.
--
-- For 'hPutSome', an 'ErrorStreamWithCorruption' is provided to simulate
-- corruption.
--
-- An 'Errors' is used in conjunction with 'SimErrorFS', which is a layer on
-- top of 'SimFS' that simulates methods throwing 'FsError's.
data Errors = Errors
  { dumpStateE                :: ErrorStream -- TODO remove
    -- Operations on files
  , hOpenE                    :: ErrorStream
  , hCloseE                   :: ErrorStream
  , hSeekE                    :: ErrorStream
  , hGetSomeE                 :: ErrorStreamGetSome
  , hGetSomeAtE               :: ErrorStreamGetSome
  , hPutSomeE                 :: ErrorStreamPutSome
  , hTruncateE                :: ErrorStream
  , hGetSizeE                 :: ErrorStream
    -- Operations on directories
  , createDirectoryE          :: ErrorStream
  , createDirectoryIfMissingE :: ErrorStream
  , listDirectoryE            :: ErrorStream
  , doesDirectoryExistE       :: ErrorStream
  , doesFileExistE            :: ErrorStream
  , removeDirectoryRecursiveE :: ErrorStream
  , removeFileE               :: ErrorStream
  , renameFileE               :: ErrorStream
    -- File I\/O with user-supplied buffers
  , hGetBufSomeE              :: ErrorStreamGetSome
  , hGetBufSomeAtE            :: ErrorStreamGetSome
  , hPutBufSomeE              :: ErrorStreamPutSome
  , hPutBufSomeAtE            :: ErrorStreamPutSome
  }
$(pure []) -- https://blog.monadfix.com/th-groups

-- | Return 'True' if all streams are empty ('null').
allNull :: Errors -> Bool
allNull $(fields 'Errors) = and [
      Stream.null dumpStateE
    , Stream.null hOpenE
    , Stream.null hCloseE
    , Stream.null hSeekE
    , Stream.null hGetSomeE
    , Stream.null hGetSomeAtE
    , Stream.null hPutSomeE
    , Stream.null hTruncateE
    , Stream.null hGetSizeE
    , Stream.null createDirectoryE
    , Stream.null createDirectoryIfMissingE
    , Stream.null listDirectoryE
    , Stream.null doesDirectoryExistE
    , Stream.null doesFileExistE
    , Stream.null removeDirectoryRecursiveE
    , Stream.null removeFileE
    , Stream.null renameFileE
      -- File I\/O with user-supplied buffers
    , Stream.null hGetBufSomeE, Stream.null hGetBufSomeAtE
    , Stream.null hPutBufSomeE, Stream.null hPutBufSomeAtE
    ]

instance Show Errors where
  show $(fields 'Errors) =
      "Errors {"  <> intercalate ", " streams <> "}"
    where
      -- | Show a stream unless it is empty
      s :: Show a => String -> Stream a -> Maybe String
      s fld str | Stream.null str = Nothing
                | otherwise       = Just $ fld <> " = " <> show str

      streams :: [String]
      streams = catMaybes
        [ s "dumpStateE"                dumpStateE
        , s "hOpenE"                    hOpenE
        , s "hCloseE"                   hCloseE
        , s "hSeekE"                    hSeekE
        , s "hGetSomeE"                 hGetSomeE
        , s "hGetSomeAtE"               hGetSomeAtE
        , s "hPutSomeE"                 hPutSomeE
        , s "hTruncateE"                hTruncateE
        , s "hGetSizeE"                 hGetSizeE
        , s "createDirectoryE"          createDirectoryE
        , s "createDirectoryIfMissingE" createDirectoryIfMissingE
        , s "listDirectoryE"            listDirectoryE
        , s "doesDirectoryExistE"       doesDirectoryExistE
        , s "doesFileExistE"            doesFileExistE
        , s "removeDirectoryRecursiveE" removeDirectoryRecursiveE
        , s "removeFileE"               removeFileE
        , s "renameFileE"               renameFileE
          -- File I\/O with user-supplied buffers
        , s "hGetBufSomeE"   hGetBufSomeE
        , s "hGetBufSomeAtE" hGetBufSomeAtE
        , s "hPutBufSomeE"   hPutBufSomeE
        , s "hPutBufSomeAtE" hPutBufSomeAtE
        ]

emptyErrors :: Errors
emptyErrors = simpleErrors Stream.empty

-- | Use the given 'ErrorStream' for each field/method. No corruption of
-- 'hPutSome'.
simpleErrors :: ErrorStream -> Errors
simpleErrors es = Errors
    { dumpStateE                = es
    , hOpenE                    = es
    , hCloseE                   = es
    , hSeekE                    = es
    , hGetSomeE                 = Left               <$> es
    , hGetSomeAtE               = Left               <$> es
    , hPutSomeE                 = Left . (, Nothing) <$> es
    , hTruncateE                = es
    , hGetSizeE                 = es
    , createDirectoryE          = es
    , createDirectoryIfMissingE = es
    , listDirectoryE            = es
    , doesDirectoryExistE       = es
    , doesFileExistE            = es
    , removeDirectoryRecursiveE = es
    , removeFileE               = es
    , renameFileE               = es
      -- File I\/O with user-supplied buffers
    , hGetBufSomeE   = Left <$> es
    , hGetBufSomeAtE = Left <$> es
    , hPutBufSomeE   = Left . (, Nothing) <$> es
    , hPutBufSomeAtE = Left . (, Nothing) <$> es
    }

-- | Generator for 'Errors' that allows some things to be disabled.
--
-- This is needed by the VolatileDB state machine tests, which try to predict
-- what should happen based on the 'Errors', which is too complex sometimes.
genErrors :: Bool  -- ^ 'True' -> generate partial writes
          -> Bool  -- ^ 'True' -> generate 'SubstituteWithJunk' corruptions
          -> Gen Errors
genErrors genPartialWrites genSubstituteWithJunk = do
    let -- TODO which errors are possible for these operations below (that
        -- have dummy for now)?
        dummy = streamGen 2 [ FsInsufficientPermissions ]
    dumpStateE          <- dummy
    -- TODO let this one fail:
    let hCloseE = Stream.empty
    hTruncateE          <- dummy
    doesDirectoryExistE <- dummy
    doesFileExistE      <- dummy
    hOpenE <- streamGen 1
      [ FsResourceDoesNotExist, FsResourceInappropriateType
      , FsResourceAlreadyInUse, FsResourceAlreadyExist
      , FsInsufficientPermissions, FsTooManyOpenFiles ]
    hSeekE      <- streamGen 3 [ FsReachedEOF ]
    hGetSomeE   <- commonGetErrors
    hGetSomeAtE <- commonGetErrors
    hPutSomeE   <- commonPutErrors
    hGetSizeE   <- streamGen 2 [ FsResourceDoesNotExist ]
    createDirectoryE <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceAlreadyExist ]
    createDirectoryIfMissingE <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceAlreadyExist ]
    listDirectoryE <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceDoesNotExist ]
    removeDirectoryRecursiveE <- streamGen 3
      [ FsInsufficientPermissions, FsResourceAlreadyInUse
      , FsResourceDoesNotExist, FsResourceInappropriateType ]
    removeFileE    <- streamGen 3
      [ FsInsufficientPermissions, FsResourceAlreadyInUse
      , FsResourceDoesNotExist, FsResourceInappropriateType ]
    renameFileE    <- streamGen 3
      [ FsInsufficientPermissions, FsResourceAlreadyInUse
      , FsResourceDoesNotExist, FsResourceInappropriateType ]
    -- File I\/O with user-supplied buffers
    hGetBufSomeE   <- commonGetErrors
    hGetBufSomeAtE <- commonGetErrors
    hPutBufSomeE   <- commonPutErrors
    hPutBufSomeAtE <- commonPutErrors
    return Errors {..}
  where
    streamGen l = Stream.genInfinite . Stream.genMaybe' l . QC.elements
    streamGen' l = Stream.genInfinite . Stream.genMaybe' l . QC.frequency

    commonGetErrors = streamGen' 20
      [ (1, return $ Left FsReachedEOF)
      , (3, Right <$> arbitrary) ]

    commonPutErrors = streamGen' 5
      [ (1, Left . (FsDeviceFull, ) <$> QC.frequency
            [ (2, return Nothing)
            , (1, Just . PartialWrite <$> arbitrary)
            , (if genSubstituteWithJunk then 1 else 0,
               Just . SubstituteWithJunk <$> arbitrary)
            ])
      , (if genPartialWrites then 3 else 0, Right <$> arbitrary) ]

instance Arbitrary Errors where
  arbitrary = genErrors True True

  shrink err@($(fields 'Errors))
    | allNull err = []
    | otherwise = emptyErrors : concatMap (filter (not . allNull))
        [ (\s' -> err { dumpStateE = s' })                <$> Stream.shrinkStream dumpStateE
        , (\s' -> err { hOpenE = s' })                    <$> Stream.shrinkStream hOpenE
        , (\s' -> err { hCloseE = s' })                   <$> Stream.shrinkStream hCloseE
        , (\s' -> err { hSeekE = s' })                    <$> Stream.shrinkStream hSeekE
        , (\s' -> err { hGetSomeE = s' })                 <$> Stream.shrinkStream hGetSomeE
        , (\s' -> err { hGetSomeAtE = s' })               <$> Stream.shrinkStream hGetSomeAtE
        , (\s' -> err { hPutSomeE = s' })                 <$> Stream.shrinkStream hPutSomeE
        , (\s' -> err { hTruncateE = s' })                <$> Stream.shrinkStream hTruncateE
        , (\s' -> err { hGetSizeE = s' })                 <$> Stream.shrinkStream hGetSizeE
        , (\s' -> err { createDirectoryE = s' })          <$> Stream.shrinkStream createDirectoryE
        , (\s' -> err { createDirectoryIfMissingE = s' }) <$> Stream.shrinkStream createDirectoryIfMissingE
        , (\s' -> err { listDirectoryE = s' })            <$> Stream.shrinkStream listDirectoryE
        , (\s' -> err { doesDirectoryExistE = s' })       <$> Stream.shrinkStream doesDirectoryExistE
        , (\s' -> err { doesFileExistE = s' })            <$> Stream.shrinkStream doesFileExistE
        , (\s' -> err { removeDirectoryRecursiveE = s' }) <$> Stream.shrinkStream removeDirectoryRecursiveE
        , (\s' -> err { removeFileE = s' })               <$> Stream.shrinkStream removeFileE
        , (\s' -> err { renameFileE = s' })               <$> Stream.shrinkStream renameFileE
          -- File I\/O with user-supplied buffers
        , (\s' -> err { hGetBufSomeE = s' })   <$> Stream.shrinkStream hGetBufSomeE
        , (\s' -> err { hGetBufSomeAtE = s' }) <$> Stream.shrinkStream hGetBufSomeAtE
        , (\s' -> err { hPutBufSomeE = s' })   <$> Stream.shrinkStream hPutBufSomeE
        , (\s' -> err { hPutBufSomeAtE = s' }) <$> Stream.shrinkStream hPutBufSomeAtE
        ]

{-------------------------------------------------------------------------------
  Simulate Errors monad
-------------------------------------------------------------------------------}

-- | Alternative to 'simErrorHasFS' that creates 'TVar's internally.
simErrorHasFS' :: (MonadSTM m, MonadThrow m, PrimMonad m)
                 => MockFS
                 -> Errors
                 -> m (HasFS m HandleMock)
simErrorHasFS' mockFS errs =
    simErrorHasFS <$> newTMVarIO mockFS <*> newTVarIO errs

-- | Introduce possibility of errors
simErrorHasFS :: forall m. (MonadSTM m, MonadThrow m, PrimMonad m)
                => StrictTMVar m MockFS
                -> StrictTVar m Errors
                -> HasFS m HandleMock
simErrorHasFS fsVar errorsVar =
    -- TODO: Lenses would be nice for the setters
    case Sim.simHasFS fsVar of
      hfs@HasFS{..} -> HasFS{
          dumpState =
            withErr errorsVar (mkFsPath ["<dumpState>"]) dumpState "dumpState"
              dumpStateE (\e es -> es { dumpStateE = e })
        , hOpen      = \p m ->
            withErr errorsVar p (hOpen p m) "hOpen"
            hOpenE (\e es -> es { hOpenE = e })
        , hClose     = \h ->
            withErr' errorsVar h (hClose h) "hClose"
            hCloseE (\e es -> es { hCloseE = e })
        , hIsOpen    = hIsOpen
        , hSeek      = \h m n ->
            withErr' errorsVar h (hSeek h m n) "hSeek"
            hSeekE (\e es -> es { hSeekE = e })
        , hGetSome   = hGetSome' errorsVar hGetSome
        , hGetSomeAt = hGetSomeAt' errorsVar hGetSomeAt
        , hPutSome   = hPutSome' errorsVar hPutSome
        , hTruncate  = \h w ->
            withErr' errorsVar h (hTruncate h w) "hTruncate"
            hTruncateE (\e es -> es { hTruncateE = e })
        , hGetSize   =  \h ->
            withErr' errorsVar h (hGetSize h) "hGetSize"
            hGetSizeE (\e es -> es { hGetSizeE = e })

        , createDirectory          = \p ->
            withErr errorsVar p (createDirectory p) "createDirectory"
            createDirectoryE (\e es -> es { createDirectoryE = e })
        , createDirectoryIfMissing = \b p ->
            withErr errorsVar p (createDirectoryIfMissing b p) "createDirectoryIfMissing"
            createDirectoryIfMissingE (\e es -> es { createDirectoryIfMissingE = e })
        , listDirectory            = \p ->
            withErr errorsVar p (listDirectory p) "listDirectory"
            listDirectoryE (\e es -> es { listDirectoryE = e })
        , doesDirectoryExist       = \p ->
            withErr errorsVar p (doesDirectoryExist p) "doesDirectoryExist"
            doesDirectoryExistE (\e es -> es { doesDirectoryExistE = e })
        , doesFileExist            = \p ->
            withErr errorsVar p (doesFileExist p) "doesFileExist"
            doesFileExistE (\e es -> es { doesFileExistE = e })
        , removeDirectoryRecursive = \p ->
            withErr errorsVar p (removeDirectoryRecursive p) "removeFile"
            removeDirectoryRecursiveE (\e es -> es { removeDirectoryRecursiveE = e })
        , removeFile               = \p ->
            withErr errorsVar p (removeFile p) "removeFile"
            removeFileE (\e es -> es { removeFileE = e })
        , renameFile               = \p1 p2 ->
            withErr errorsVar p1 (renameFile p1 p2) "renameFile"
            renameFileE (\e es -> es { renameFileE = e })
        , mkFsErrorPath = fsToFsErrorPathUnmounted
        , unsafeToFilePath = error "simErrorHasFS:unsafeToFilePath"
          -- File I\/O with user-supplied buffers
        , hGetBufSome   = hGetBufSomeWithErr   errorsVar hfs
        , hGetBufSomeAt = hGetBufSomeAtWithErr errorsVar hfs
        , hPutBufSome   = hPutBufSomeWithErr   errorsVar hfs
        , hPutBufSomeAt = hPutBufSomeAtWithErr errorsVar hfs
        }

-- | Runs a computation provided an 'Errors' and an initial
-- 'MockFS', producing a result and the final state of the filesystem.
runSimErrorFS :: (MonadSTM m, MonadThrow m, PrimMonad m)
              => MockFS
              -> Errors
              -> (StrictTVar m Errors -> HasFS m HandleMock -> m a)
              -> m (a, MockFS)
runSimErrorFS mockFS errors action = do
    fsVar     <- newTMVarIO mockFS
    errorsVar <- newTVarIO errors
    a         <- action errorsVar $ simErrorHasFS fsVar errorsVar
    fs'       <- atomically $ takeTMVar fsVar
    return (a, fs')

-- | Execute the next action using the given 'Errors'. After the action is
-- finished, the previous 'Errors' are restored.
withErrors :: MonadSTM m => StrictTVar m Errors -> Errors -> m a -> m a
withErrors errorsVar tempErrors action = do
    originalErrors <- atomically $ do
      originalErrors <- readTVar errorsVar
      writeTVar errorsVar tempErrors
      return originalErrors
    res <- action
    atomically $ writeTVar errorsVar originalErrors
    return res

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Advance to the next error in the stream of some 'ErrorStream' in the
-- 'Errors' stored in the 'StrictTVar'. Extracts the right error stream from
-- the state with the @getter@ and stores the advanced error stream in the
-- state with the @setter@.
next :: MonadSTM m
     => StrictTVar m Errors
     -> (Errors -> Stream a)            -- ^ @getter@
     -> (Stream a -> Errors -> Errors)  -- ^ @setter@
     -> m (Maybe a)
next errorsVar getter setter = do
    atomically $ do
      errors <- readTVar errorsVar
      let (mb, s') = Stream.runStream (getter errors)
      writeTVar errorsVar (setter s' errors)
      return mb

-- | Execute an action or throw an error, depending on the corresponding
-- 'ErrorStream' (see 'nextError').
withErr :: (MonadSTM m, MonadThrow m, HasCallStack)
        => StrictTVar m Errors
        -> FsPath     -- ^ The path for the error, if thrown
        -> m a        -- ^ Action in case no error is thrown
        -> String     -- ^ Extra message for in the 'fsErrorString'
        -> (Errors -> ErrorStream)           -- ^ @getter@
        -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
        -> m a
withErr errorsVar path action msg getter setter = do
    mbErr <- next errorsVar getter setter
    case mbErr of
      Nothing      -> action
      Just errType -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted path
        , fsErrorString = "simulated error: " <> msg
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }

-- | Variant of 'withErr' that works with 'Handle's.
--
-- The path of the handle is retrieved from the 'MockFS' using 'handleFsPath'.
withErr' :: (MonadSTM m, MonadThrow m, HasCallStack)
         => StrictTVar m Errors
         -> Handle HandleMock   -- ^ The path for the error, if thrown
         -> m a        -- ^ Action in case no error is thrown
         -> String     -- ^ Extra message for in the 'fsErrorString'
         -> (Errors -> ErrorStream)           -- ^ @getter@
         -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
         -> m a
withErr' errorsVar handle action msg getter setter =
    withErr errorsVar (handlePath handle) action msg getter setter

-- | Execute the wrapped 'hGetSome', throw an error, or simulate a partial
-- read, depending on the corresponding 'ErrorStreamGetSome' (see
-- 'nextError').
hGetSome'  :: (MonadSTM m, MonadThrow m, HasCallStack)
           => StrictTVar m Errors
           -> (Handle HandleMock -> Word64 -> m BS.ByteString)  -- ^ Wrapped 'hGetSome'
           -> Handle HandleMock -> Word64 -> m BS.ByteString
hGetSome' errorsVar hGetSomeWrapped handle n =
    next errorsVar hGetSomeE (\e es -> es { hGetSomeE = e }) >>= \case
      Nothing             -> hGetSomeWrapped handle n
      Just (Left errType) -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath handle
        , fsErrorString = "simulated error: hGetSome"
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }
      Just (Right partial) ->
        hGetSomeWrapped handle (partialiseWord64 partial n)

-- | In the thread safe version of 'hGetSome', we simulate exactly the same errors.
hGetSomeAt' :: (MonadSTM m, MonadThrow m, HasCallStack)
            => StrictTVar m Errors
            -> (Handle HandleMock -> Word64 -> AbsOffset -> m BS.ByteString)  -- ^ Wrapped 'hGetSomeAt'
            -> Handle HandleMock -> Word64 -> AbsOffset -> m BS.ByteString
hGetSomeAt' errorsVar hGetSomeAtWrapped handle n offset =
    next errorsVar hGetSomeAtE (\e es -> es { hGetSomeAtE = e }) >>= \case
      Nothing             -> hGetSomeAtWrapped handle n offset
      Just (Left errType) -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath handle
        , fsErrorString = "simulated error: hGetSomeAt"
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }
      Just (Right partial) ->
        hGetSomeAtWrapped handle (partialiseWord64 partial n) offset

-- | Execute the wrapped 'hPutSome', throw an error and apply possible
-- corruption to the blob to write, or simulate a partial write, depending on
-- the corresponding 'ErrorStreamPutSome' (see 'nextError').
hPutSome' :: (MonadSTM m, MonadThrow m, HasCallStack)
          => StrictTVar m Errors
          -> (Handle HandleMock -> BS.ByteString -> m Word64)  -- ^ Wrapped 'hPutSome'
          -> Handle HandleMock -> BS.ByteString -> m Word64
hPutSome' errorsVar hPutSomeWrapped handle bs =
    next errorsVar hPutSomeE (\e es -> es { hPutSomeE = e }) >>= \case
      Nothing                       -> hPutSomeWrapped handle bs
      Just (Left (errType, mbCorr)) -> do
        for_ mbCorr $ \corr ->
          void $ hPutSomeWrapped handle (corruptByteString bs corr)
        throwIO FsError
          { fsErrorType   = errType
          , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath handle
          , fsErrorString = "simulated error: hPutSome" <> case mbCorr of
              Nothing   -> ""
              Just corr -> " with corruption: " <> show corr
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
      Just (Right partial)          ->
        hPutSomeWrapped handle (partialiseByteString partial bs)

{-------------------------------------------------------------------------------
  File I\/O with user-supplied buffers
-------------------------------------------------------------------------------}

-- | Short-hand for the type of 'hGetBufSome'
type HGetBufSome m =
     Handle HandleMock
  -> MutableByteArray (PrimState m)
  -> BufferOffset
  -> ByteCount
  -> m ByteCount

-- | Execute the wrapped 'hGetBufSome', throw an error, or simulate a partial
-- read, depending on the corresponding 'ErrorStreamGetSome' (see 'nextError').
hGetBufSomeWithErr  ::
     (MonadSTM m, MonadThrow m, HasCallStack)
  => StrictTVar m Errors
  -> HasFS m HandleMock
  -> HGetBufSome m
hGetBufSomeWithErr errorsVar hfs h buf bufOff c =
    next errorsVar hGetBufSomeE (\e es -> es { hGetBufSomeE = e }) >>= \case
      Nothing             -> hGetBufSome hfs h buf bufOff c
      Just (Left errType) -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath h
        , fsErrorString = "simulated error: hGetBufSome"
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }
      Just (Right partial) ->
        hGetBufSome hfs h buf bufOff (partialiseByteCount partial c)

-- | Short-hand for the type of 'hGetBufSomeAt'
type HGetBufSomeAt m =
     Handle HandleMock
  -> MutableByteArray (PrimState m)
  -> BufferOffset
  -> ByteCount
  -> AbsOffset
  -> m ByteCount

-- | Execute the wrapped 'hGetBufSomeAt', throw an error, or simulate a partial
-- read, depending on the corresponding 'ErrorStreamGetSome' (see 'nextError').
hGetBufSomeAtWithErr  ::
     (MonadSTM m, MonadThrow m, HasCallStack)
  => StrictTVar m Errors
  -> HasFS m HandleMock
  -> HGetBufSomeAt m
hGetBufSomeAtWithErr errorsVar hfs h buf bufOff c off =
    next errorsVar hGetBufSomeAtE (\e es -> es { hGetBufSomeAtE = e }) >>= \case
      Nothing             -> hGetBufSomeAt hfs h buf bufOff c off
      Just (Left errType) -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath h
        , fsErrorString = "simulated error: hGetBufSomeAt"
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }
      Just (Right partial) ->
        hGetBufSomeAt hfs h buf bufOff (partialiseByteCount partial c) off

-- | Short-hand for the type of 'hPutBufSome'
type HPutBufSome m =
     Handle HandleMock
  -> MutableByteArray (PrimState m)
  -> BufferOffset
  -> ByteCount
  -> m ByteCount

-- | Execute the wrapped 'hPutBufSome', throw an error and apply possible
-- corruption to the blob to write, or simulate a partial write, depending on
-- the corresponding 'ErrorStreamPutSome' (see 'nextError').
hPutBufSomeWithErr ::
     (MonadSTM m, MonadThrow m, PrimMonad m, HasCallStack)
  => StrictTVar m Errors
  -> HasFS m HandleMock
  -> HPutBufSome m
hPutBufSomeWithErr errorsVar hfs h buf bufOff c =
    next errorsVar hPutBufSomeE (\e es -> es { hPutBufSomeE = e }) >>= \case
      Nothing                       -> hPutBufSome hfs h buf bufOff c
      Just (Left (errType, mbCorr)) -> do
        for_ mbCorr $ \corr -> do
          (buf', c') <- corruptBuffer buf bufOff c corr
          void $ hPutBufSome hfs h buf' bufOff c'
        throwIO FsError
          { fsErrorType   = errType
          , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath h
          , fsErrorString = "simulated error: hPutSome" <> case mbCorr of
              Nothing   -> ""
              Just corr -> " with corruption: " <> show corr
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
      Just (Right partial)          ->
        hPutBufSome hfs h buf bufOff (partialiseByteCount partial c)

-- | Short-hand for the type of 'hPutBufSomeAt'
type HPutBufSomeAt m =
     Handle HandleMock
  -> MutableByteArray (PrimState m)
  -> BufferOffset
  -> ByteCount
  -> AbsOffset
  -> m ByteCount

-- | Execute the wrapped 'hPutBufSomeAt', throw an error and apply possible
-- corruption to the blob to write, or simulate a partial write, depending on
-- the corresponding 'ErrorStreamPutSome' (see 'nextError').
hPutBufSomeAtWithErr ::
     (MonadSTM m, MonadThrow m, PrimMonad m, HasCallStack)
  => StrictTVar m Errors
  -> HasFS m HandleMock
  -> HPutBufSomeAt m
hPutBufSomeAtWithErr errorsVar hfs h buf bufOff c off =
    next errorsVar hPutBufSomeAtE (\e es -> es { hPutBufSomeAtE = e }) >>= \case
      Nothing                       -> hPutBufSomeAt hfs h buf bufOff c off
      Just (Left (errType, mbCorr)) -> do
        for_ mbCorr $ \corr -> do
          (buf', c') <- corruptBuffer buf bufOff c corr
          void $ hPutBufSomeAt hfs h buf' bufOff c' off
        throwIO FsError
          { fsErrorType   = errType
          , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath h
          , fsErrorString = "simulated error: hPutSome" <> case mbCorr of
              Nothing   -> ""
              Just corr -> " with corruption: " <> show corr
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
      Just (Right partial)          ->
        hPutBufSomeAt hfs h buf bufOff (partialiseByteCount partial c) off
