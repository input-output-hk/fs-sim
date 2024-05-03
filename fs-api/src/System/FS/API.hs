{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
    -- * File I\/O with user-supplied buffers
  , BufferOffset (..)
  , hGetBufExactly
  , hGetBufExactlyAt
  , hPutBufExactly
  , hPutBufExactlyAt
  ) where

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Primitive (PrimMonad (..))
import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Data.Primitive (MutableByteArray)
import           Data.Set (Set)
import           Data.Word
import           System.Posix.Types (ByteCount)

import           System.FS.API.Types as Types
import           System.FS.CallStack

{------------------------------------------------------------------------------
  Record that abstracts over the filesystem
------------------------------------------------------------------------------}

-- | Abstract interface for performing file I\/O
--
-- [User-supplied buffers #user-supplied-buffers#]: For functions that require
--     user-supplied buffers (i.e., 'MutableByteArray'), it is the user's
--     responsiblity to provide buffers that are large enough. Behaviour is
--     undefined if the I\/O operations access the buffer outside it's allocated
--     range.
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
    -- Postcondition: for the length of the returned bytestring @bs@ we have
    -- @length bs >= 0@ and @length bs <= n@.
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
    -- Postcondition: the return value @n@ is @n > 0@ and @n <= l@, unless the
    -- given bytestring is empty, in which case @n@ can be 0.
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

    -- === File I\/O with user-supplied buffers

    -- | Like 'hGetSome', but the bytes are read into a user-supplied buffer.
    -- See [__User-supplied buffers__](#user-supplied-buffers).
  , hGetBufSome   :: HasCallStack
                  => Handle h
                  -> MutableByteArray (PrimState m) -- ^ Buffer to read bytes into
                  -> BufferOffset -- ^ Offset into buffer
                  -> ByteCount -- ^ The number of bytes to read
                  -> m ByteCount
    -- | Like 'hGetSomeAt', but the bytes are read into a user-supplied buffer.
    -- See [__User-supplied buffers__](#user-supplied-buffers).
  , hGetBufSomeAt :: HasCallStack
                  => Handle h
                  -> MutableByteArray (PrimState m)   -- ^ Buffer to read bytes into
                  -> BufferOffset -- ^ Offset into buffer
                  -> ByteCount -- ^ The number of bytes to read
                  -> AbsOffset -- ^ The file offset at which to read
                  -> m ByteCount
    -- | Like 'hPutSome', but the bytes are written from a user-supplied buffer.
    -- See [__User-supplied buffers__](#user-supplied-buffers).
  , hPutBufSome   :: HasCallStack
                  => Handle h
                  -> MutableByteArray (PrimState m) -- ^ Buffer to write bytes from
                  -> BufferOffset -- ^ Offset into buffer
                  -> ByteCount -- ^ The number of bytes to write
                  -> m ByteCount
    -- | Like 'hPutSome', but the bytes are written from a user-supplied buffer
    -- at a given file offset. This offset does not affect the offset stored in
    -- the file handle (see also 'hGetSomeAt'). See [__User-supplied buffers__](#user-supplied-buffers).
  , hPutBufSomeAt :: HasCallStack
                  => Handle h
                  -> MutableByteArray (PrimState m)   -- ^ Buffer to write bytes from
                  -> BufferOffset -- ^ Offset into buffer
                  -> ByteCount -- ^ The number of bytes to write
                  -> AbsOffset -- ^ The file offset at which to write
                  -> m ByteCount
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

{-------------------------------------------------------------------------------
  File I\/O with user-supplied buffers
-------------------------------------------------------------------------------}

-- | Absolute offset into a buffer (i.e., 'MutableByteArray').
--
-- Can be negative, because buffer offsets can be added together to change
-- offset positions. This is similar to 'plusPtr' for 'Ptr' types. However, note
-- that reading or writing from a buffer at a negative offset leads to undefined
-- behaviour.
newtype BufferOffset = BufferOffset { unBufferOffset :: Int }
  deriving (Eq, Ord, Enum, Bounded, Num, Show)

-- | Wrapper for 'hGetBufSome' that ensures that we read exactly as many
-- bytes as requested. If EOF is found before the requested number of bytes is
-- read, an 'FsError' exception is thrown.
hGetBufExactly :: forall m h. (HasCallStack, MonadThrow m)
               => HasFS m h
               -> Handle h
               -> MutableByteArray (PrimState m) -- ^ Buffer to read bytes into
               -> BufferOffset -- ^ Offset into buffer
               -> ByteCount  -- ^ The number of bytes to read
               -> m ByteCount
hGetBufExactly hfs h buf bufOff c = go c bufOff
  where
    go :: ByteCount -> BufferOffset -> m ByteCount
    go !remainingCount !currentBufOff
      | remainingCount == 0 = pure c
      | otherwise            = do
          readBytes <- hGetBufSome hfs h buf currentBufOff c
          if readBytes == 0 then
            throwIO FsError {
                fsErrorType   = FsReachedEOF
              , fsErrorPath   = mkFsErrorPath hfs $ handlePath h
              , fsErrorString = "hGetBufExactly found eof before reading " ++ show c ++ " bytes"
              , fsErrorNo     = Nothing
              , fsErrorStack  = prettyCallStack
              , fsLimitation  = False
              }
          -- We know the length <= remainingBytes, so this can't underflow.
          else go (remainingCount - readBytes)
                  (currentBufOff + fromIntegral readBytes)

-- | Wrapper for 'hGetBufSomeAt' that ensures that we read exactly as many bytes
-- as requested. If EOF is found before the requested number of bytes is read,
-- an 'FsError' exception is thrown.
hGetBufExactlyAt :: forall m h. (HasCallStack, MonadThrow m)
                 => HasFS m h
                 -> Handle h
                 -> MutableByteArray (PrimState m)   -- ^ Buffer to read bytes into
                 -> BufferOffset -- ^ Offset into buffer
                 -> ByteCount -- ^ The number of bytes to read
                 -> AbsOffset -- ^ The file offset at which to read
                 -> m ByteCount
hGetBufExactlyAt hfs h buf bufOff c off = go c off bufOff
  where
    go :: ByteCount -> AbsOffset -> BufferOffset -> m ByteCount
    go !remainingCount !currentOffset !currentBufOff
      | remainingCount == 0 = pure c
      | otherwise            = do
          readBytes <- hGetBufSomeAt hfs h buf currentBufOff c currentOffset
          if readBytes == 0 then
            throwIO FsError {
                fsErrorType   = FsReachedEOF
              , fsErrorPath   = mkFsErrorPath hfs $ handlePath h
              , fsErrorString = "hGetBufExactlyAt found eof before reading " ++ show c ++ " bytes"
              , fsErrorNo     = Nothing
              , fsErrorStack  = prettyCallStack
              , fsLimitation  = False
              }
          -- We know the length <= remainingBytes, so this can't underflow.
          else go (remainingCount - readBytes)
                  (currentOffset + fromIntegral readBytes)
                  (currentBufOff + fromIntegral readBytes)

-- | Wrapper for 'hPutBufSome' that ensures we write exactly as many bytes as
-- requested.
hPutBufExactly :: forall m h. (HasCallStack, MonadThrow m)
                 => HasFS m h
                 -> Handle h
                 -> MutableByteArray (PrimState m) -- ^ Buffer to write bytes from
                 -> BufferOffset -- ^ Offset into buffer
                 -> ByteCount  -- ^ The number of bytes to write
                 -> m ByteCount
hPutBufExactly hbfs h buf bufOff c = go c bufOff
  where
    go :: ByteCount -> BufferOffset -> m ByteCount
    go !remainingCount !currentBufOff = do
      writtenBytes <- hPutBufSome hbfs h buf currentBufOff remainingCount
      let remainingCount' = remainingCount - writtenBytes
      if remainingCount' == 0
        then pure c
        else go remainingCount'
                (currentBufOff + fromIntegral writtenBytes)

-- | Wrapper for 'hPutBufSomeAt' that ensures we write exactly as many bytes as
-- requested.
hPutBufExactlyAt :: forall m h. (HasCallStack, MonadThrow m)
                 => HasFS m h
                 -> Handle h
                 -> MutableByteArray (PrimState m)   -- ^ Buffer to write bytes from
                 -> BufferOffset -- ^ Offset into buffer
                 -> ByteCount -- ^ The number of bytes to write
                 -> AbsOffset -- ^ The file offset at which to write
                 -> m ByteCount
hPutBufExactlyAt hbfs h buf bufOff c off = go c off bufOff
  where
    go :: ByteCount -> AbsOffset -> BufferOffset -> m ByteCount
    go !remainingCount !currentOffset !currentBufOff = do
      writtenBytes <- hPutBufSomeAt hbfs h buf currentBufOff remainingCount currentOffset
      let remainingCount' = remainingCount - writtenBytes
      if remainingCount' == 0
        then pure c
        else go remainingCount'
                (currentOffset + fromIntegral writtenBytes)
                (currentBufOff + fromIntegral writtenBytes)
