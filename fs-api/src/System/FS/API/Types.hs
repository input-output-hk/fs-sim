{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module System.FS.API.Types (
    -- * Modes
    AllowExisting (..)
  , OpenMode (..)
  , SeekMode (..)
  , allowExisting
    -- * Paths
  , MountPoint (..)
  , fsFromFilePath
  , fsPathFromList
  , fsPathInit
  , fsPathSplit
  , fsPathToList
  , fsToFilePath
  , mkFsPath
  , (<.>)
  , addExtension
  , (</>)
  , combine
    -- ** opaque
  , FsPath
    -- * Handles
  , Handle (..)
    -- * Offset
  , AbsOffset (..)
    -- * Errors
  , FsError (..)
  , FsErrorPath (..)
  , FsErrorType (..)
  , fsToFsErrorPath
  , fsToFsErrorPathUnmounted
  , hasMountPoint
  , isFsErrorType
  , prettyFsError
  , sameFsError
    -- * From 'IOError' to 'FsError'
  , ioToFsError
  , ioToFsErrorType
  ) where

import           Control.DeepSeq (NFData (..), force)
import           Control.Exception
import           Data.Function (on)
import           Data.List (intercalate, stripPrefix)
import           Data.Maybe (isJust)
import qualified Data.Text as Strict
import qualified Data.Text as Text
import           Data.Word
import           Foreign.C.Error (Errno (..))
import qualified Foreign.C.Error as C
import           GHC.Generics (Generic)
import qualified GHC.IO.Exception as GHC
import           GHC.Show (showCommaSpace)
import qualified System.FilePath as FilePath
import           System.IO (SeekMode (..))
import qualified System.IO.Error as IO

import           System.FS.CallStack
import           System.FS.Condense

{-------------------------------------------------------------------------------
  Modes
-------------------------------------------------------------------------------}

-- | How to 'System.FS.API.hOpen' a new file.
data OpenMode
  = ReadMode
  | WriteMode     AllowExisting
  | AppendMode    AllowExisting
  | ReadWriteMode AllowExisting
  deriving (Eq, Show)

-- | When opening a file:
data AllowExisting
  = AllowExisting
    -- ^ The file may already exist. If it does, it is reopened. If it
    -- doesn't, it is created.
  | MustBeNew
    -- ^ The file may not yet exist. If it does, an error
    -- ('FsResourceAlreadyExist') is thrown.
  deriving (Eq, Show)

allowExisting :: OpenMode -> AllowExisting
allowExisting openMode = case openMode of
  ReadMode         -> AllowExisting
  WriteMode     ex -> ex
  AppendMode    ex -> ex
  ReadWriteMode ex -> ex

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

-- | A relative path.
--
-- === Invariant
--
-- The user of this library is tasked with picking sensible names of
-- directories/files on a path. Amongst others, the following should hold:
--
-- * Names are non-empty
--
-- * Names are monotonic, i.e., they are not equal to @..@
--
-- * Names should not contain path separators or drive letters
--
-- In particular, names that satisfy these invariants should result in an
-- 'FsPath' that remains relative to the HasFS instance root. For example, an
-- @'FsPath' ["/"]@ would try to access the root folder, which is most likely
-- outside of the scope of the HasFS instance.
--
-- \"@..@\" should not be used because @fs-sim@ will not be able to follow these
-- types of back-links. @fs-sim@ will interpret \"@..@\" as a directory name
-- instead.
newtype FsPath = UnsafeFsPath { fsPathToList :: [Strict.Text] }
  deriving (Eq, Ord, Generic)
  deriving newtype NFData

-- | Create a path from a list of directory/file names. All of the names should
-- be non-empty.
fsPathFromList :: [Strict.Text] -> FsPath
fsPathFromList xs = UnsafeFsPath (force xs)

instance Show FsPath where
  show = intercalate "/" . map Strict.unpack . fsPathToList

instance Condense FsPath where
  condense = show

-- | Constructor for 'FsPath' ensures path is in normal form
mkFsPath :: [String] -> FsPath
mkFsPath = fsPathFromList . map Strict.pack

-- | Split 'FsPath' is essentially @(init fp, last fp)@
--
-- Like @init@ and @last@, 'Nothing' if empty.
fsPathSplit :: FsPath -> Maybe (FsPath, Strict.Text)
fsPathSplit fp =
    case reverse (fsPathToList fp) of
      []   -> Nothing
      p:ps -> Just (fsPathFromList (reverse ps), p)

-- | Drop the final component of the path
--
-- Undefined if the path is empty.
fsPathInit :: HasCallStack => FsPath -> FsPath
fsPathInit fp = case fsPathSplit fp of
                  Nothing       -> error $ "fsPathInit: empty path"
                  Just (fp', _) -> fp'

-- | An alias for '<.>'.
addExtension :: FsPath -> String -> FsPath
addExtension = (<.>)

infixr 7 <.>
-- | Add an extension, even if there is already one there.
--
-- This works similarly to 'Filepath.<.>'.
(<.>) :: FsPath -> String -> FsPath
path <.> [] = path
path <.> ext = case fsPathSplit path of
    Nothing          -> mkFsPath [ext']
    Just (dir, file) -> dir </> UnsafeFsPath [file `Text.append` Text.pack ext']
  where
    ext' = case ext of
      '.':_ -> ext
      _     -> '.':ext

-- | An alias for '</>'.
combine :: FsPath -> FsPath -> FsPath
combine = (</>)

infixr 5 </>
-- | Combine two paths with a path separator.
--
-- This works similarly to 'Filepath.</>', but since the arguments are
-- relative paths, the corner cases for 'FilePath.</>' do not apply.
-- Specifically, the second path will never start with a path separator or a
-- drive letter, so the result is simply the concatenation of the two paths.
--
-- If either operand is empty, the other operand is returned. The result of
-- combining two empty paths is the empty path
(</>) :: FsPath -> FsPath -> FsPath
UnsafeFsPath x </> UnsafeFsPath y = case (x, y) of
    ([], _) -> UnsafeFsPath y
    (_, []) -> UnsafeFsPath x
    _       -> fsPathFromList (x ++ y)

-- | Mount point
--
-- 'FsPath's are not absolute paths, but must be interpreted with respect to
-- a particualar mount point.
newtype MountPoint = MountPoint FilePath

fsToFilePath :: MountPoint -> FsPath -> FilePath
fsToFilePath (MountPoint mp) fp =
    mp FilePath.</> foldr (FilePath.</>) "" (map Strict.unpack $ fsPathToList fp)

fsFromFilePath :: MountPoint -> FilePath -> Maybe FsPath
fsFromFilePath (MountPoint mp) path = mkFsPath <$>
    stripPrefix (FilePath.splitDirectories mp) (FilePath.splitDirectories path)

-- | For better error reporting to the end user, we want to include the
-- mount point of the file. But the mountpoint may not always be available,
-- like when we mock the fs or we simulate fs errors.
data FsErrorPath = FsErrorPath (Maybe MountPoint) FsPath

fsToFsErrorPath :: MountPoint -> FsPath -> FsErrorPath
fsToFsErrorPath mp = FsErrorPath (Just mp)

-- | Like 'fsToFsErrorPath', but when we don't have a 'MountPoint'
fsToFsErrorPathUnmounted :: FsPath -> FsErrorPath
fsToFsErrorPathUnmounted = FsErrorPath Nothing

instance Show FsErrorPath where
  show (FsErrorPath (Just mp) fp) = fsToFilePath mp fp
  show (FsErrorPath Nothing   fp) = show fp

instance Condense FsErrorPath where
  condense = show

-- | We only care to compare the 'FsPath', because the 'MountPoint' may not
-- exist.
instance Eq FsErrorPath where
  (FsErrorPath _ fp1) == (FsErrorPath _ fp2) = fp1 == fp2

{-------------------------------------------------------------------------------
  Handles
-------------------------------------------------------------------------------}

data Handle h = Handle {
      -- | The raw underlying handle
      handleRaw  :: !h

      -- | The path corresponding to this handle
      --
      -- This is primarily useful for error reporting.
    , handlePath :: !FsPath
    }
  deriving (Generic)

instance NFData h => NFData (Handle h) where
    rnf (Handle handleRaw handlePath) = rnf handleRaw `seq` rnf handlePath

instance Eq h => Eq (Handle h) where
  (==) = (==) `on` handleRaw

instance Show (Handle h) where
  show (Handle _ fp) = "<Handle " ++ fsToFilePath (MountPoint "<root>") fp ++ ">"


{-------------------------------------------------------------------------------
  Offset wrappers
-------------------------------------------------------------------------------}

newtype AbsOffset = AbsOffset { unAbsOffset :: Word64 }
  deriving (Eq, Ord, Enum, Bounded, Num, Show)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data FsError = FsError {
      -- | Error type
      fsErrorType   :: FsErrorType

      -- | Path to the file
    , fsErrorPath   :: FsErrorPath

      -- | Human-readable string giving additional information about the error
    , fsErrorString :: String

      -- | The 'Errno', if available. This is more precise than the
      -- 'FsErrorType'.
    , fsErrorNo     :: Maybe Errno

      -- | Call stack
    , fsErrorStack  :: PrettyCallStack

      -- | Is this error due to a limitation of the mock file system?
      --
      -- The mock file system does not all of Posix's features and quirks.
      -- This flag will be set for such unsupported IO calls. Real I/O calls
      -- would not have thrown an error for these calls.
    , fsLimitation  :: Bool
    }

-- This is a custom instance and not an auto-derive one, since 'Errno' does not
-- have a 'Show' instance, and we don't want to provide an orphan instance for
-- this @base@ type.
instance Show FsError where
  showsPrec n fserr = showParen (n >= 11) $
        showString "FsError {"
      . showString "fsErrorType = " . shows fsErrorType . showCommaSpace
      . showString "fsErrorPath = " . shows fsErrorPath . showCommaSpace
      . showString "fsErrorString = " . shows fsErrorString . showCommaSpace
      . showString "fsErrorNo = " . showsFsErrNo fsErrorNo . showCommaSpace
      . showString "fsErrorStack = " . shows fsErrorStack . showCommaSpace
      . showString "fsLimitation = " . shows fsLimitation
      . showString "}"
    where
      -- Quite a bit of boilerplate, but it should ensure that we won't silently
      -- change/forget to change the Show instance when fields are
      -- changed/re-ordered/added.
      FsError {
          fsErrorType = fsErrorType :: FsErrorType
        , fsErrorPath = fsErrorPath :: FsErrorPath
        , fsErrorString = fsErrorString :: String
        , fsErrorNo = fsErrorNo :: Maybe Errno
        , fsErrorStack = fsErrorStack :: PrettyCallStack
        , fsLimitation = fsLimitation :: Bool
        } = fserr
      _coveredAllCases = case fserr of
        FsError (_ :: FsErrorType) (_ :: FsErrorPath) (_ :: String)
                (_ :: Maybe Errno) (_ :: PrettyCallStack) (_ :: Bool) -> ()

      showsFsErrNo Nothing          = showString "Nothing"
      showsFsErrNo (Just (Errno e)) = showString "Just "
                                    . showParen True (showString "Errno " . shows e)

data FsErrorType
  = FsIllegalOperation
  | FsResourceInappropriateType
  -- ^ e.g the user tried to open a directory with hOpen rather than a file.
  | FsResourceAlreadyInUse
  | FsResourceDoesNotExist
  | FsResourceAlreadyExist
  | FsReachedEOF
  | FsDeviceFull
  | FsTooManyOpenFiles
  | FsInsufficientPermissions
  | FsInvalidArgument
  | FsOther
    -- ^ Used for all other error types
  deriving (Show, Eq)

instance Exception FsError where
    displayException = prettyFsError

-- | Check if two errors are semantically the same error
--
-- This ignores the error string, the errno, and the callstack.
sameFsError :: FsError -> FsError -> Bool
sameFsError e e' = fsErrorType e == fsErrorType e'
                && fsErrorPath e == fsErrorPath e'

isFsErrorType :: FsErrorType -> FsError -> Bool
isFsErrorType ty e = fsErrorType e == ty

prettyFsError :: FsError -> String
prettyFsError FsError{..} = concat [
      show fsErrorType
    , " for "
    , show fsErrorPath
    , ": "
    , fsErrorString
    , " at "
    , show fsErrorStack
    ]

hasMountPoint :: FsError -> Bool
hasMountPoint FsError{fsErrorPath = FsErrorPath mp _} = isJust mp

{-------------------------------------------------------------------------------
  From 'IOError' to 'FsError'
-------------------------------------------------------------------------------}

-- | Translate exceptions thrown by IO functions to 'FsError'
--
-- We take the 'FsPath' as an argument. We could try to translate back from a
-- 'FilePath' to an 'FsPath' (given a 'MountPoint'), but we know the 'FsPath'
-- at all times anyway and not all IO exceptions actually include a filepath.
ioToFsError :: HasCallStack
            => FsErrorPath -> IOError -> FsError
ioToFsError fep ioErr = FsError
    { fsErrorType   = ioToFsErrorType ioErr
    , fsErrorPath   = fep
      -- We don't use 'ioeGetErrorString', because that only returns the
      -- description in case 'isUserErrorType' is true, otherwise it will
      -- return 'ioToFsErrorType', which we already include in 'fsErrorType'.
      -- So we use the underlying field directly.
    , fsErrorString = GHC.ioe_description ioErr
    , fsErrorNo     = Errno <$> GHC.ioe_errno ioErr
    , fsErrorStack  = prettyCallStack
    , fsLimitation  = False
    }

-- | Assign an 'FsErrorType' to the given 'IOError'.
--
-- Note that we don't always use the classification made by
-- 'Foreign.C.Error.errnoToIOError' (also see 'System.IO.Error') because it
-- combines some errors into one 'IO.IOErrorType', e.g., @EMFILE@ (too many open
-- files) and @ENOSPC@ (no space left on device) both result in
-- 'ResourceExhausted' while we want to keep them separate. For this reason,
-- we do a classification of our own based on the @errno@ while sometimes
-- deferring to the existing classification.
--
-- See the ERRNO(3) man page for the meaning of the different errnos.
ioToFsErrorType :: IOError -> FsErrorType
ioToFsErrorType ioErr = case Errno <$> GHC.ioe_errno ioErr of
    Just errno
      |  errno == C.eACCES
      || errno == C.eROFS
      || errno == C.ePERM
      -> FsInsufficientPermissions

      |  errno == C.eNOSPC
      -> FsDeviceFull

      |  errno == C.eMFILE
      || errno == C.eNFILE
      -> FsTooManyOpenFiles

      |  errno == C.eNOENT
      || errno == C.eNXIO
      -> FsResourceDoesNotExist

    _ | IO.isAlreadyInUseErrorType eType
      -> FsResourceAlreadyInUse

      | IO.isAlreadyExistsErrorType eType
      -> FsResourceAlreadyExist

      | IO.isEOFErrorType eType
      -> FsReachedEOF

      | IO.isIllegalOperationErrorType eType
      -> FsIllegalOperation

      | eType == GHC.InappropriateType
      -> FsResourceInappropriateType

      | eType == GHC.InvalidArgument
      -> FsInvalidArgument

      | otherwise
      -> FsOther
  where
    eType :: IO.IOErrorType
    eType = IO.ioeGetErrorType ioErr

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense AllowExisting where
  condense AllowExisting = ""
  condense MustBeNew     = "!"

instance Condense OpenMode where
    condense ReadMode           = "r"
    condense (WriteMode     ex) = "w"  ++ condense ex
    condense (ReadWriteMode ex) = "rw" ++ condense ex
    condense (AppendMode    ex) = "a"  ++ condense ex

instance Condense (Handle h) where
  condense = show
