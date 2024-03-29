{-# OPTIONS_GHC -Wno-dodgy-imports #-}

-- | This is meant to be used for the implementation of HasFS instances and not
-- directly by client code.
module System.FS.IO.Internal (
    FHandle
  , close
  , getSize
  , open
  , pread
  , preadBuf
  , pwriteBuf
  , read
  , readBuf
  , sameError
  , seek
  , truncate
  , write
  , writeBuf
  ) where

import           Prelude hiding (read, truncate)

import           Control.Monad (void)
import           Data.Bits ((.|.))
import           Data.ByteString
import           Data.ByteString.Internal as Internal
import           Data.Word (Word32, Word64, Word8)
import           Foreign (Int64, Ptr)
import           System.FS.API.Types (AllowExisting (..), FsError (..),
                     FsErrorType (..), OpenMode (..), SeekMode (..))
import           System.FS.IO.Internal.Handle
import           System.Posix.Types
import           System.Win32

type FHandle = HandleOS HANDLE

open :: FilePath -> OpenMode -> IO HANDLE
open filename openMode = do
    h <- createFile filename
                    accessMode
                    (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
                    Nothing
                    creationDisposition
                    fILE_ATTRIBUTE_NORMAL
                    Nothing
    -- There is no AppendMode in Windows, so we manually seek to the end of
    -- the file. For now we don't need to carry a flag that this handle is in
    -- AppendMode, but we may need to if we add more commands (read and seek
    -- are disabled in AppendMode and truncate only works in AppendMode, write
    -- moves the file offset in all modes).
    case openMode of
      AppendMode{} -> void $ setFilePointerEx h 0 fILE_END
      _            -> return ()
    return h
  where
    (accessMode, creationDisposition) = case openMode of
      ReadMode         -> (gENERIC_READ,                   oPEN_EXISTING)
      AppendMode    ex -> (                 gENERIC_WRITE, createNew ex)
      WriteMode     ex -> (gENERIC_READ .|. gENERIC_WRITE, createNew ex)
      ReadWriteMode ex -> (gENERIC_READ .|. gENERIC_WRITE, createNew ex)
    createNew AllowExisting = oPEN_ALWAYS
    createNew MustBeNew     = cREATE_NEW

write :: FHandle -> Ptr Word8 -> Int64 -> IO Word32
write fh data' bytes = withOpenHandle "write" fh $ \h ->
  win32_WriteFile h data' (fromIntegral bytes) Nothing

seek :: FHandle -> SeekMode -> Int64 -> IO ()
seek fh seekMode size = void <$> withOpenHandle "seek" fh $ \h ->
  setFilePointerEx h size (fromSeekMode seekMode)

fromSeekMode :: SeekMode -> FilePtrDirection
fromSeekMode AbsoluteSeek = fILE_BEGIN
fromSeekMode RelativeSeek = fILE_CURRENT
fromSeekMode SeekFromEnd  = fILE_END

read :: FHandle -> Word64 -> IO ByteString
read fh bytes = withOpenHandle "read" fh $ \h ->
  Internal.createUptoN (fromIntegral bytes) $ \ptr ->
    fromIntegral <$> win32_ReadFile h ptr (fromIntegral bytes) Nothing

getCurrentFileOffset :: HANDLE -> IO Int64
getCurrentFileOffset h = setFilePointerEx h 0 fILE_CURRENT

readBuf :: FHandle -> Ptr Word8 -> ByteCount -> IO ByteCount
readBuf fh buf c = withOpenHandle "readBuf" fh $ \h ->
    fromIntegral <$> win32_ReadFile h buf (fromIntegral c) Nothing

writeBuf :: FHandle -> Ptr Word8 -> ByteCount -> IO ByteCount
writeBuf fh buf c = withOpenHandle "writeBuf" fh $ \h ->
    fromIntegral <$> win32_WriteFile h buf (fromIntegral c) Nothing

pread :: FHandle -> Word64 -> Word64 -> IO ByteString
pread fh bytes pos = withOpenHandle "pread" fh $ \h ->
  Internal.createUptoN (fromIntegral bytes) $ \ptr -> do
    initialOffset <- getCurrentFileOffset h
    _ <- setFilePointerEx h (fromIntegral pos) fILE_BEGIN
    n <- fromIntegral <$> win32_ReadFile h ptr (fromIntegral bytes) Nothing
    _ <- setFilePointerEx h initialOffset fILE_BEGIN
    return n

preadBuf :: FHandle -> Ptr Word8 -> ByteCount -> FileOffset -> IO ByteCount
preadBuf fh buf c off = withOpenHandle "preadBuf" fh $ \h -> do
    initialOffset <- getCurrentFileOffset h
    _ <- setFilePointerEx h (fromIntegral off) fILE_BEGIN
    n <- fromIntegral <$> win32_ReadFile h buf (fromIntegral c) Nothing
    _ <- setFilePointerEx h initialOffset fILE_BEGIN
    return n

pwriteBuf :: FHandle -> Ptr Word8 -> ByteCount -> FileOffset -> IO ByteCount
pwriteBuf fh buf c off = withOpenHandle "pwriteBuf" fh $ \h -> do
    initialOffset <- getCurrentFileOffset h
    _ <- setFilePointerEx h (fromIntegral off) fILE_BEGIN
    n <- fromIntegral <$> win32_WriteFile h buf (fromIntegral c) Nothing
    _ <- setFilePointerEx h initialOffset fILE_BEGIN
    return n

-- We only allow truncate in AppendMode, but Windows do not support it, so we manually seek to the end.
-- It is important that the logical end of the handle stays alligned to the physical end of the file.
truncate :: FHandle -> Word64 -> IO ()
truncate fh size =  withOpenHandle "truncate" fh $ \h -> do
  _ <- setFilePointerEx h (fromIntegral size) (fromSeekMode AbsoluteSeek)
  setEndOfFile h

close :: FHandle -> IO ()
close fh = closeHandleOS fh closeHandle

getSize :: FHandle -> IO Word64
getSize fh = withOpenHandle "getSize" fh $ \h ->
  bhfiSize <$> getFileInformationByHandle h

-- | For the following error types, our mock FS implementation (and the Posix
-- implementation) throw the same errors:
--
-- * 'FsReachedEOF'
-- * 'FsDeviceFull'
-- * 'FsResourceAlreadyInUse'
--
-- For other cases, Windows throws different errors than the mock FS
-- implementation.
sameError :: FsError -> FsError -> Bool
sameError e1 e2 = fsErrorPath e1 == fsErrorPath e2
               && sameFsErrorType (fsErrorType e1) (fsErrorType e2)
  where
    sameFsErrorType ty1 ty2 = case (ty1, ty2) of
      (FsReachedEOF,           FsReachedEOF)           -> True
      (FsReachedEOF,           _)                      -> False
      (_,                      FsReachedEOF)           -> False
      (FsDeviceFull,           FsDeviceFull)           -> True
      (FsDeviceFull,           _)                      -> False
      (_,                      FsDeviceFull)           -> False
      (FsResourceAlreadyInUse, FsResourceAlreadyInUse) -> True
      (FsResourceAlreadyInUse, _)                      -> False
      (_,                      FsResourceAlreadyInUse) -> False
      (_,                      _)                      -> True
