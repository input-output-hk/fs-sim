{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | IO implementation of the 'HasFS' class
module System.FS.IO (
    -- * IO implementation & monad
    HandleIO
  , ioHasFS
  , ioHasBufFS
  ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Primitive (PrimMonad (..))
import qualified Data.ByteString.Unsafe as BS
import           Data.Primitive (withMutableByteArrayContents)
import qualified Data.Set as Set
import qualified Foreign
import           GHC.Stack
import qualified System.Directory as Dir
import           System.FS.API
import qualified System.FS.IO.Internal as F
import qualified System.FS.IO.Internal.Handle as H

{-------------------------------------------------------------------------------
  I/O implementation of HasFS
-------------------------------------------------------------------------------}

-- | File handlers for the IO instance for HasFS
--
-- We store the path the handle points to for better error messages
type HandleIO = F.FHandle

ioHasFS :: MonadIO m => MountPoint -> HasFS m HandleIO
ioHasFS mount = HasFS {
      -- TODO(adn) Might be useful to implement this properly by reading all
      -- the stuff available at the 'MountPoint'.
      dumpState = return "<dumpState@IO>"
    , hOpen = \fp openMode -> liftIO $ do
        let path = root fp
        osHandle <- rethrowFsError fp $
            F.open path openMode
        hVar <- newMVar $ Just osHandle
        return $ Handle (H.HandleOS path hVar) fp
    , hClose = \(Handle h fp) -> liftIO $ rethrowFsError fp $
        F.close h
    , hIsOpen = liftIO . H.isOpenHandleOS . handleRaw
    , hSeek = \(Handle h fp) mode o -> liftIO $ rethrowFsError fp $
        F.seek h mode o
    , hGetSome = \(Handle h fp) n -> liftIO $ rethrowFsError fp $
        F.read h n
    , hGetSomeAt = \(Handle h fp) n o -> liftIO $ rethrowFsError fp $
        F.pread h n (unAbsOffset o)
    , hTruncate = \(Handle h fp) sz -> liftIO $ rethrowFsError fp $
        F.truncate h sz
    , hGetSize = \(Handle h fp) -> liftIO $ rethrowFsError fp $
        F.getSize h
    , hPutSome = \(Handle h fp) bs -> liftIO $ rethrowFsError fp $ do
        BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
            fromIntegral <$> F.write h (Foreign.castPtr ptr) (fromIntegral len)
    , createDirectory = \fp -> liftIO $ rethrowFsError fp $
        Dir.createDirectory (root fp)
    , listDirectory = \fp -> liftIO $ rethrowFsError fp $
        Set.fromList <$>  Dir.listDirectory (root fp)
    , doesDirectoryExist= \fp -> liftIO $ rethrowFsError fp $
        Dir.doesDirectoryExist (root fp)
    , doesFileExist = \fp -> liftIO $ rethrowFsError fp $
        Dir.doesFileExist (root fp)
    , createDirectoryIfMissing = \createParent fp -> liftIO $ rethrowFsError fp $
        Dir.createDirectoryIfMissing createParent (root fp)
    , removeDirectoryRecursive = \fp -> liftIO $ rethrowFsError fp $
        Dir.removeDirectoryRecursive (root fp)
    , removeFile = \fp -> liftIO $ rethrowFsError fp $
        Dir.removeFile (root fp)
    , renameFile = \fp1 fp2 -> liftIO $ rethrowFsError fp1 $
        Dir.renameFile (root fp1) (root fp2)
    , mkFsErrorPath = fsToFsErrorPath mount
    , unsafeToFilePath = pure . root
    }
  where
    root :: FsPath -> FilePath
    root = fsToFilePath mount

    rethrowFsError :: HasCallStack => FsPath -> IO a -> IO a
    rethrowFsError = _rethrowFsError mount

{-# INLINE _rethrowFsError #-}
-- | Catch IO exceptions and rethrow them as 'FsError'
--
-- See comments for 'ioToFsError'
_rethrowFsError :: HasCallStack => MountPoint -> FsPath -> IO a -> IO a
_rethrowFsError mount fp action = do
    res <- E.try action
    case res of
      Left err -> handleError err
      Right a  -> return a
  where
    handleError :: HasCallStack => IOError -> IO a
    handleError ioErr = E.throwIO $ ioToFsError errorPath ioErr

    errorPath :: FsErrorPath
    errorPath = fsToFsErrorPath mount fp

{-------------------------------------------------------------------------------
  HasBufFS
-------------------------------------------------------------------------------}

ioHasBufFS ::
     (MonadIO m, PrimState IO ~ PrimState m)
  => MountPoint
  -> HasBufFS m HandleIO
ioHasBufFS mount = HasBufFS {
      hGetBufSome = \(Handle h fp) buf bufOff c ->  liftIO $ rethrowFsError fp $
        withMutableByteArrayContents buf $ \ptr ->
          F.readBuf h (ptr `Foreign.plusPtr` unBufferOffset bufOff) c
    , hGetBufSomeAt = \(Handle h fp) buf bufOff c off -> liftIO $ rethrowFsError fp $
        withMutableByteArrayContents buf $ \ptr ->
          F.preadBuf h (ptr `Foreign.plusPtr` unBufferOffset bufOff) c (fromIntegral $ unAbsOffset off)
    , hPutBufSome = \(Handle h fp) buf bufOff c -> liftIO $ rethrowFsError fp $
        withMutableByteArrayContents buf $ \ptr ->
          F.writeBuf h (ptr `Foreign.plusPtr` unBufferOffset bufOff) c
    , hPutBufSomeAt = \(Handle h fp) buf bufOff c off -> liftIO $ rethrowFsError fp $
        withMutableByteArrayContents buf $ \ptr ->
          F.pwriteBuf h (ptr `Foreign.plusPtr` unBufferOffset bufOff) c (fromIntegral $ unAbsOffset off)
    }
  where
    rethrowFsError = _rethrowFsError mount
