{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An abstract view over the filesystem.
module System.FS.API (
    -- * Record that abstracts over the filesystem
    HasFS (..)
    -- * Types
  , module Types
    -- * Opening and closing files
  , hClose'
  , withFile
    -- * SomeHasFS
  , SomeHasFS (..)
  ) where

import           Control.Monad.Class.MonadThrow
import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Data.Set (Set)
import           Data.Word

import           System.FS.API.Types as Types

import           Util.CallStack

{------------------------------------------------------------------------------
  Record that abstracts over the filesystem
------------------------------------------------------------------------------}

data HasFS m h = HasFS {
    -- | Debugging: human-readable description of file system state
    dumpState                :: m String

    -- Operations of files

    -- | Open a file
  , hOpen                    :: HasCallStack => FsPath -> OpenMode -> m (Handle h)

    -- | Close a file
  , hClose                   :: HasCallStack => Handle h -> m ()

    -- | Is the handle open?
  , hIsOpen                  :: HasCallStack => Handle h -> m Bool

    -- | Seek handle
    --
    -- The offset is an 'Int64' rather than a 'Word64' because it may be
    -- negative (for use in relative positioning).
    --
    -- Unlike the Posix @lseek@, 'hSeek' does not return the new seek position
    -- because the value returned by Posix is rather strange and unreliable
    -- and we don't want to emulate it's behaviour.
  , hSeek                    :: HasCallStack => Handle h -> SeekMode -> Int64 -> m ()

    -- | Try to read @n@ bytes from a handle
    --
    -- When at the end of the file, an empty bytestring will be returned.
    --
    -- The returned bytestring will typically have length @n@, but may be
    -- shorter in case of a partial read, see #277. However, a partial read
    -- will always return at least 1 byte, as returning 0 bytes would mean
    -- that we have reached EOF.
    --
    -- Postcondition: the length of the returned bytestring <= @n@ and >= 0.
  , hGetSome                 :: HasCallStack => Handle h -> Word64 -> m BS.ByteString

    -- | Same as 'hGetSome', but does not affect the file offset. An additional argument
    -- is used to specify the offset. This allows it to be called concurrently for the
    -- same file handle. However, the actual level of parallelism achieved depends on
    -- the implementation and the operating system: generally on Unix it will be
    -- \"more parallel\" than on Windows.
  , hGetSomeAt               :: HasCallStack
                             => Handle h
                             -> Word64    -- The number of bytes to read.
                             -> AbsOffset -- The offset at which to read.
                             -> m BS.ByteString

    -- | Write to a handle
    --
    -- The return value indicates the number of bytes written and will
    -- typically be equal to @l@, the length of the bytestring, but may be
    -- shorter in case of a partial write, see #277.
    --
    -- If nothing can be written at all, an exception will be thrown.
    --
    -- Postcondition: the return value <= @l@ and > 0, unless the given
    -- bytestring is empty, in which case the return value can be 0.
  , hPutSome                 :: HasCallStack => Handle h -> BS.ByteString -> m Word64

    -- | Truncate the file to the specified size
    --
    -- NOTE: Only supported in append mode.
  , hTruncate                :: HasCallStack => Handle h -> Word64 -> m ()

    -- | Return current file size
    --
    -- NOTE: This is not thread safe (changes made to the file in other threads
    -- may affect this thread).
  , hGetSize                 :: HasCallStack => Handle h -> m Word64

    -- Operations of directories

    -- | Create new directory
  , createDirectory          :: HasCallStack => FsPath -> m ()

    -- | Create new directory if it doesn't exist.
    --
    -- @createDirectoryIfMissing True@ will also try to create all parent dirs.
  , createDirectoryIfMissing :: HasCallStack => Bool -> FsPath -> m ()

    -- | List contents of a directory
  , listDirectory            :: HasCallStack => FsPath -> m (Set String)

    -- | Check if the path exists and is a directory
  , doesDirectoryExist       :: HasCallStack => FsPath -> m Bool

    -- | Check if the path exists and is a file
  , doesFileExist            :: HasCallStack => FsPath -> m Bool

    -- | Remove the directory (which must exist) and its contents
  , removeDirectoryRecursive :: HasCallStack => FsPath -> m ()

    -- | Remove the file (which must exist)
  , removeFile               :: HasCallStack => FsPath -> m ()

    -- | Rename the file (which must exist) from the first path to the second
    -- path. If there is already a file at the latter path, it is replaced by
    -- the new one.
    --
    -- NOTE: only works for files within the same folder.
  , renameFile               :: HasCallStack => FsPath -> FsPath -> m ()

    -- | Useful for better error reporting
  , mkFsErrorPath            :: FsPath -> FsErrorPath

    -- | Create an absolute 'FilePath' from a relative 'FsPath'.
    --
    -- This is an escape hatch for creating absolute paths when @m ~'IO'@.
    --
    -- Postcondition: Should throw an error for any @m@ that is not @IO@
    -- (or for which we do not have @'MonadIO' m@).
  , unsafeToFilePath         :: FsPath -> m FilePath
  }

{-------------------------------------------------------------------------------
  Opening and closing files
-------------------------------------------------------------------------------}

withFile :: (HasCallStack, MonadThrow m)
         => HasFS m h -> FsPath -> OpenMode -> (Handle h -> m a) -> m a
withFile HasFS{..} fp openMode = bracket (hOpen fp openMode) hClose

-- | Returns 'True' when the handle was still open.
hClose' :: (HasCallStack, Monad m) => HasFS m h -> Handle h -> m Bool
hClose' HasFS { hClose, hIsOpen } h = do
    isOpen <- hIsOpen h
    if isOpen then do
      hClose h
      return True
    else
      return False

{-------------------------------------------------------------------------------
  SomeHasFS
-------------------------------------------------------------------------------}

-- | It is often inconvenient to have to parameterise over @h@. This data type
-- hides an existential @h@ parameter of a 'HasFS'.
data SomeHasFS m where
  SomeHasFS :: Eq h => HasFS m h -> SomeHasFS m
