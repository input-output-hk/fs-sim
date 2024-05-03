{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.FS.API.Lazy (
    -- * API
    module API
    -- * Lazy functions
  , hGetAll
  , hGetAllAt
  , hGetExactly
  , hGetExactlyAt
  , hPut
  , hPutAll
  ) where

import           Control.Monad (foldM)
import           Control.Monad.Class.MonadThrow (MonadThrow (throwIO))
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word64)
import           System.FS.API as API
import           System.FS.API.Strict
import           System.FS.CallStack (HasCallStack, prettyCallStack)

-- | Makes sure it reads all requested bytes.
-- If eof is found before all bytes are read, it throws an exception.
hGetExactly :: forall m h. (HasCallStack, MonadThrow m)
            => HasFS m h
            -> Handle h
            -> Word64
            -> m BL.ByteString
hGetExactly hasFS h n = go n []
  where
    go :: Word64 -> [BS.ByteString] -> m BL.ByteString
    go remainingBytes acc
      | remainingBytes == 0 = return $ BL.fromChunks $ reverse acc
      | otherwise           = do
        bs <- hGetSome hasFS h remainingBytes
        if BS.null bs then
          throwIO FsError {
              fsErrorType   = FsReachedEOF
            , fsErrorPath   = mkFsErrorPath hasFS $ handlePath h
            , fsErrorString = "hGetExactly found eof before reading " ++ show n ++ " bytes"
            , fsErrorNo     = Nothing
            , fsErrorStack  = prettyCallStack
            , fsLimitation  = False
            }
        -- We know the length <= remainingBytes, so this can't underflow
        else go (remainingBytes - fromIntegral (BS.length bs)) (bs : acc)

-- | Like 'hGetExactly', but is thread safe since it does not change or depend
-- on the file offset. @pread@ syscall is used internally.
hGetExactlyAt :: forall m h. (HasCallStack, MonadThrow m)
              => HasFS m h
              -> Handle h
              -> Word64    -- ^ The number of bytes to read.
              -> AbsOffset -- ^ The offset at which to read.
              -> m BL.ByteString
hGetExactlyAt hasFS h n offset = go n offset []
  where
    go :: Word64 -> AbsOffset -> [BS.ByteString] -> m BL.ByteString
    go remainingBytes currentOffset acc
      | remainingBytes == 0 = return $ BL.fromChunks $ reverse acc
      | otherwise           = do
        bs <- hGetSomeAt hasFS h remainingBytes currentOffset
        let readBytes = BS.length bs
        if BS.null bs then
          throwIO FsError {
              fsErrorType   = FsReachedEOF
            , fsErrorPath   = mkFsErrorPath hasFS $ handlePath h
            , fsErrorString = "hGetExactlyAt found eof before reading " ++ show n ++ " bytes"
            , fsErrorNo     = Nothing
            , fsErrorStack  = prettyCallStack
            , fsLimitation  = False
            }
        -- We know the length <= remainingBytes, so this can't underflow.
        else go (remainingBytes - fromIntegral readBytes)
                (currentOffset + fromIntegral readBytes)
                (bs : acc)

-- | Read all the data from the given file handle 64kB at a time.
--
-- Stops when EOF is reached.
hGetAll :: Monad m => HasFS m h -> Handle h -> m BL.ByteString
hGetAll HasFS{..} hnd = go mempty
  where
    bufferSize = 64 * 1024
    go acc = do
      chunk <- hGetSome hnd bufferSize
      let acc' = chunk : acc
      if BS.null chunk
        then return $ BL.fromChunks $ reverse acc'
        else go acc'

-- | Like 'hGetAll', but is thread safe since it does not change or depend
-- on the file offset. @pread@ syscall is used internally.
hGetAllAt :: Monad m
          => HasFS m h
          -> Handle h
          -> AbsOffset -- ^ The offset at which to read.
          -> m BL.ByteString
hGetAllAt HasFS{..} hnd = go mempty
  where
    bufferSize = 64 * 1024
    go acc offset = do
      chunk <- hGetSomeAt hnd bufferSize offset
      let acc' = chunk : acc
      if BS.null chunk
        then return $ BL.fromChunks $ reverse acc'
        else go acc' (offset + fromIntegral (BS.length chunk))

-- | This function makes sure that the whole 'BL.ByteString' is written.
hPutAll :: forall m h
        .  (HasCallStack, Monad m)
        => HasFS m h
        -> Handle h
        -> BL.ByteString
        -> m Word64
hPutAll hasFS h = foldM putChunk 0 . BL.toChunks
  where
    putChunk :: Word64 -> BS.ByteString -> m Word64
    putChunk written chunk = do
      written' <- hPutAllStrict hasFS h chunk
      return $! written + written'

-- | This function makes sure that the whole 'Builder' is written.
--
-- The chunk size of the resulting 'BL.ByteString' determines how much memory
-- will be used while writing to the handle.
hPut :: forall m h
     .  (HasCallStack, Monad m)
     => HasFS m h
     -> Handle h
     -> Builder
     -> m Word64
hPut hasFS g = hPutAll hasFS g . BS.toLazyByteString
