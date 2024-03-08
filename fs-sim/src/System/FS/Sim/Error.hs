{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | 'HasFS' instance wrapping 'SimFS' that generates errors, suitable for
-- testing error handling.
module System.FS.Sim.Error (
    -- * Simulate Errors monad
    mkSimErrorHasFS
  , mkSimErrorHasFS'
  , runSimErrorFS
  , withErrors
    -- * Streams
  , ErrorStream
  , ErrorStreamGetSome
  , ErrorStreamPutSome
    -- * Generating partial reads/writes
  , Partial (..)
  , hGetSomePartial
  , hPutSomePartial
    -- * Blob
  , Blob (..)
  , blobFromBS
  , blobToBS
    -- * Generating corruption for 'hPutSome'
  , PutCorruption (..)
  , corrupt
    -- * Error streams for 'HasFS'
  , Errors (..)
  , allNull
  , emptyErrors
  , genErrors
  , simpleErrors
  ) where

import           Prelude hiding (null)

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad (void)
import           Control.Monad.Class.MonadThrow hiding (handle)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Foldable (for_)
import           Data.List (intercalate)
import qualified Data.List as List
import           Data.Maybe (catMaybes)
import           Data.String (IsString (..))
import           Data.Word (Word64)

import qualified Test.QuickCheck as QC
import           Test.QuickCheck (ASCIIString (..), Arbitrary (..), Gen,
                     suchThat)

import           Util.CallStack

import           System.FS.API

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

{-------------------------------------------------------------------------------
  Generating partial reads/writes for hGetSome and hPutSome
-------------------------------------------------------------------------------}

-- | Given a @'Partial' p@ where @p > 0@, we do the following to make a call
-- to 'hGetSome' or 'hPutSome' partial:
--
-- * 'hGetSome': we subtract @p@ from the number of requested bytes. If that
--    would result in 0 requested bytes or less, we request 1 byte. If the
--    number of requested bytes was already 0, leave it untouched, as we can't
--    simulate a partial read in this case.
-- * 'hPutSome': we drop the last @p@ bytes from the bytestring. If that would
--   result in an empty bytestring, just take the first byte of the
--   bytestring. If the bytestring was already empty, leave it untouched, as
--   we can't simulate a partial write in this case.
newtype Partial = Partial Word64
    deriving (Show)

instance Arbitrary Partial where
  arbitrary = Partial <$> QC.choose (1, 100)
  shrink (Partial p) =
    [Partial p' | p' <- [1..p]]

hGetSomePartial :: Partial -> Word64 -> Word64
hGetSomePartial (Partial p) n
    | 0 <- n    = 0
    | p >= n    = 1
    | otherwise = n - p

hPutSomePartial :: Partial -> BS.ByteString -> BS.ByteString
hPutSomePartial (Partial p) bs
    | 0 <- len  = bs
    | p >= len  = BS.take 1 bs
    | otherwise = BS.take (fromIntegral (len - p)) bs
  where
    len = fromIntegral (BS.length bs)

-- | 'ErrorStream' for 'hGetSome': an error or a partial get.
type ErrorStreamGetSome = Stream (Either FsErrorType Partial)

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
corrupt :: BS.ByteString -> PutCorruption -> BS.ByteString
corrupt bs pc = case pc of
    SubstituteWithJunk blob -> getBlob blob
    PartialWrite partial    -> hPutSomePartial partial bs

-- | 'ErrorStream' for 'hPutSome': an error and possibly some corruption, or a
-- partial write.
type ErrorStreamPutSome =
  Stream (Either (FsErrorType, Maybe PutCorruption) Partial)

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
  , hTellE                    :: ErrorStream
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
  }

-- | Return 'True' if all streams are empty ('null').
allNull :: Errors -> Bool
allNull errs =
       Stream.null dumpStateE
    && Stream.null hOpenE
    && Stream.null hCloseE
    && Stream.null hSeekE
    && Stream.null hTellE
    && Stream.null hGetSomeE
    && Stream.null hGetSomeAtE
    && Stream.null hPutSomeE
    && Stream.null hTruncateE
    && Stream.null hGetSizeE
    && Stream.null createDirectoryE
    && Stream.null createDirectoryIfMissingE
    && Stream.null listDirectoryE
    && Stream.null doesDirectoryExistE
    && Stream.null doesFileExistE
    && Stream.null removeDirectoryRecursiveE
    && Stream.null removeFileE
    && Stream.null renameFileE
  where
      -- a (non-record) pattern match ensures that we are not missing any fields
    Errors
      dumpStateE
      hOpenE
      hCloseE
      hSeekE
      hTellE
      hGetSomeE
      hGetSomeAtE
      hPutSomeE
      hTruncateE
      hGetSizeE
      createDirectoryE
      createDirectoryIfMissingE
      listDirectoryE
      doesDirectoryExistE
      doesFileExistE
      removeDirectoryRecursiveE
      removeFileE
      renameFileE
      = errs

instance Show Errors where
  show errs =
      "Errors {"  <> intercalate ", " streams <> "}"
    where
      -- a (non-record) pattern match ensures that we are not missing any fields
      Errors
        dumpStateE
        hOpenE
        hCloseE
        hSeekE
        hTellE
        hGetSomeE
        hGetSomeAtE
        hPutSomeE
        hTruncateE
        hGetSizeE
        createDirectoryE
        createDirectoryIfMissingE
        listDirectoryE
        doesDirectoryExistE
        doesFileExistE
        removeDirectoryRecursiveE
        removeFileE
        renameFileE
        = errs

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
        , s "hTellE"                    hTellE
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
        , s "removeDirectyRecursiveE"   removeDirectoryRecursiveE
        , s "removeFileE"               removeFileE
        , s "renameFileE"               renameFileE
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
    , hTellE                    = es
    , hGetSomeE                 =  Left                <$> es
    , hGetSomeAtE               =  Left                <$> es
    , hPutSomeE                 = (Left . (, Nothing)) <$> es
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
    }

-- | Generator for 'Errors' that allows some things to be disabled.
--
-- This is needed by the VolatileDB state machine tests, which try to predict
-- what should happen based on the 'Errors', which is too complex sometimes.
genErrors :: Bool  -- ^ 'True' -> generate partial writes
          -> Bool  -- ^ 'True' -> generate 'SubstituteWithJunk' corruptions
          -> Gen Errors
genErrors genPartialWrites genSubstituteWithJunk = do
    let streamGen l = Stream.genInfinite . Stream.genMaybe' l . QC.elements
        streamGen' l = Stream.genInfinite . Stream.genMaybe' l . QC.frequency
        -- TODO which errors are possible for these operations below (that
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
    hTellE      <- streamGen 3 [ FsIllegalOperation ]
    hGetSomeE   <- streamGen' 20
      [ (1, return $ Left FsReachedEOF)
      , (3, Right <$> arbitrary) ]
    hGetSomeAtE <- streamGen' 20
      [ (1, return $ Left FsReachedEOF)
      , (3, Right <$> arbitrary) ]
    hPutSomeE   <- streamGen' 5
      [ (1, Left . (FsDeviceFull, ) <$> QC.frequency
            [ (2, return Nothing)
            , (1, Just . PartialWrite <$> arbitrary)
            , (if genSubstituteWithJunk then 1 else 0,
               Just . SubstituteWithJunk <$> arbitrary)
            ])
      , (if genPartialWrites then 3 else 0, Right <$> arbitrary) ]
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
    return Errors {..}

instance Arbitrary Errors where
  arbitrary = genErrors True True

  shrink err = filter (not . allNull) $ concat
      [ (\s' -> err { dumpStateE = s' })                <$> Stream.shrinkStream dumpStateE
      , (\s' -> err { hOpenE = s' })                    <$> Stream.shrinkStream hOpenE
      , (\s' -> err { hCloseE = s' })                   <$> Stream.shrinkStream hCloseE
      , (\s' -> err { hSeekE = s' })                    <$> Stream.shrinkStream hSeekE
      , (\s' -> err { hTellE = s' })                    <$> Stream.shrinkStream hTellE
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
      ]
    where
      -- a (non-record) pattern match ensures that we are not missing any fields
      Errors
        dumpStateE
        hOpenE
        hCloseE
        hSeekE
        hTellE
        hGetSomeE
        hGetSomeAtE
        hPutSomeE
        hTruncateE
        hGetSizeE
        createDirectoryE
        createDirectoryIfMissingE
        listDirectoryE
        doesDirectoryExistE
        doesFileExistE
        removeDirectoryRecursiveE
        removeFileE
        renameFileE
        = err

{-------------------------------------------------------------------------------
  Simulate Errors monad
-------------------------------------------------------------------------------}

-- | Alternative to 'mkSimErrorHasFS' that creates 'TVar's internally.
mkSimErrorHasFS' :: (MonadSTM m, MonadThrow m)
                 => MockFS
                 -> Errors
                 -> m (HasFS m HandleMock)
mkSimErrorHasFS' mockFS errs =
    mkSimErrorHasFS <$> newTVarIO mockFS <*> newTVarIO errs

-- | Introduce possibility of errors
--
-- TODO: Lenses would be nice for the setters
mkSimErrorHasFS :: forall m. (MonadSTM m, MonadThrow m)
                => StrictTVar m MockFS
                -> StrictTVar m Errors
                -> HasFS m HandleMock
mkSimErrorHasFS fsVar errorsVar =
    case Sim.simHasFS fsVar of
      HasFS{..} -> HasFS{
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
        , hTell      = \h ->
            withErr' errorsVar h (hTell h) "hTell"
            hTellE (\e es -> es { hTellE = e })
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
        , unsafeToFilePath = error "mkSimErrorHasFS:unsafeToFilePath"
        }

-- | Runs a computation provided an 'Errors' and an initial
-- 'MockFS', producing a result and the final state of the filesystem.
runSimErrorFS :: (MonadSTM m, MonadThrow m)
              => MockFS
              -> Errors
              -> (StrictTVar m Errors -> HasFS m HandleMock -> m a)
              -> m (a, MockFS)
runSimErrorFS mockFS errors action = do
    fsVar     <- newTVarIO mockFS
    errorsVar <- newTVarIO errors
    a         <- action errorsVar $ mkSimErrorHasFS fsVar errorsVar
    fs'       <- readTVarIO fsVar
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
withErr' errorsVar handle = withErr errorsVar (handlePath handle)

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
        hGetSomeWrapped handle (hGetSomePartial partial n)

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
        hGetSomeAtWrapped handle (hGetSomePartial partial n) offset

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
          void $ hPutSomeWrapped handle (corrupt bs corr)
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
        hPutSomeWrapped handle (hPutSomePartial partial bs)
