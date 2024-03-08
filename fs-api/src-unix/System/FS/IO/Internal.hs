{-# LANGUAGE CPP            #-}
{-# LANGUAGE PackageImports #-}

-- | This is meant to be used for the implementation of HasFS instances and not
-- directly by client code.
module System.FS.IO.Internal (
    FHandle
  , close
  , getSize
  , open
  , pread
  , read
  , sameError
  , seek
  , tell
  , truncate
  , write
  ) where

import           Prelude hiding (read, truncate)

import           Control.Monad (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as Internal
import           Data.Int (Int64)
import           Data.Word (Word32, Word64, Word8)
import           Foreign (Ptr)
import           System.FS.API.Types (AllowExisting (..), FsError,
                     OpenMode (..), SeekMode (..), sameFsError)
import           System.FS.IO.Internal.Handle
import qualified System.Posix as Posix
import           System.Posix (Fd (..))
import           System.Posix.IO.ByteString.Ext (fdPreadBuf)

type FHandle = HandleOS Fd

-- | Some sensible defaults for the 'OpenFileFlags'.
--
-- NOTE: the 'unix' package /already/ exports a smart constructor called
-- @defaultFileFlags@ already, but we define our own to not be depedent by
-- whichever default choice unix's library authors made, and to be able to
-- change our minds later if necessary. In particular, we are interested in the
-- 'append' and 'exclusive' flags, which were largely the reason why we
-- introduced this low-level module.
defaultFileFlags :: Posix.OpenFileFlags
defaultFileFlags = Posix.OpenFileFlags {
      Posix.append    = False
    , Posix.exclusive = False
    , Posix.noctty    = False
    , Posix.nonBlock  = False
    , Posix.trunc     = False
# if MIN_VERSION_unix(2,8,0)
    , Posix.nofollow  = False
    , Posix.creat     = Nothing
    , Posix.cloexec   = False
    , Posix.directory = False
    , Posix.sync      = False
# endif
    }

-- | Opens a file from disk.
open :: FilePath -> OpenMode -> IO Fd
# if MIN_VERSION_unix(2,8,0)
open fp openMode = Posix.openFd fp posixOpenMode fileFlags
  where
    (posixOpenMode, fileFlags) = case openMode of
      ReadMode         -> ( Posix.ReadOnly
                          , defaultFileFlags
                          )
      AppendMode    ex -> ( Posix.WriteOnly
                          , defaultFileFlags { Posix.append = True
                                             , Posix.exclusive = isExcl ex
                                             , Posix.creat = Just Posix.stdFileMode }
                          )
      ReadWriteMode ex -> ( Posix.ReadWrite
                          , defaultFileFlags { Posix.exclusive = isExcl ex
                                             , Posix.creat = Just Posix.stdFileMode }
                          )
      WriteMode     ex -> ( Posix.ReadWrite
                          , defaultFileFlags { Posix.exclusive = isExcl ex
                                             , Posix.creat = Just Posix.stdFileMode }
                          )

    isExcl AllowExisting = False
    isExcl MustBeNew     = True
# else
open fp openMode = Posix.openFd fp posixOpenMode fileMode fileFlags
  where
    (posixOpenMode, fileMode, fileFlags) = case openMode of
      ReadMode         -> ( Posix.ReadOnly
                          , Nothing
                          , defaultFileFlags
                          )
      AppendMode    ex -> ( Posix.WriteOnly
                          , Just Posix.stdFileMode
                          , defaultFileFlags { Posix.append = True
                                             , Posix.exclusive = isExcl ex }
                          )
      ReadWriteMode ex -> ( Posix.ReadWrite
                          , Just Posix.stdFileMode
                          , defaultFileFlags { Posix.exclusive = isExcl ex }
                          )
      WriteMode     ex -> ( Posix.ReadWrite
                          , Just Posix.stdFileMode
                          , defaultFileFlags { Posix.exclusive = isExcl ex }
                          )

    isExcl AllowExisting = False
    isExcl MustBeNew     = True
# endif

-- | Writes the data pointed by the input 'Ptr Word8' into the input 'FHandle'.
write :: FHandle -> Ptr Word8 -> Int64 -> IO Word32
write h data' bytes = withOpenHandle "write" h $ \fd ->
    fromIntegral <$> Posix.fdWriteBuf fd data' (fromIntegral bytes)

-- | Seek within the file.
--
-- The offset may be negative.
--
-- We don't return the new offset since the behaviour of lseek is rather odd
-- (e.g., the file pointer may not actually be moved until a subsequent write)
seek :: FHandle -> SeekMode -> Int64 -> IO ()
seek h seekMode offset = withOpenHandle "seek" h $ \fd ->
    void $ Posix.fdSeek fd seekMode (fromIntegral offset)

-- | Request the absolute file offset stored in the handle
tell :: FHandle -> IO Word64
tell h = withOpenHandle "tell" h $ \fd ->
    fromIntegral <$> Posix.fdSeek fd RelativeSeek 0

-- | Reads a given number of bytes from the input 'FHandle'.
read :: FHandle -> Word64 -> IO ByteString
read h bytes = withOpenHandle "read" h $ \fd ->
    Internal.createUptoN (fromIntegral bytes) $ \ptr ->
      fromIntegral <$> Posix.fdReadBuf fd ptr (fromIntegral bytes)

pread :: FHandle -> Word64 -> Word64 -> IO ByteString
pread h bytes offset = withOpenHandle "pread" h $ \fd ->
    Internal.createUptoN (fromIntegral bytes) $ \ptr ->
      fromIntegral <$> fdPreadBuf fd ptr (fromIntegral bytes) (fromIntegral offset)

-- | Truncates the file managed by the input 'FHandle' to the input size.
truncate :: FHandle -> Word64 -> IO ()
truncate h sz = withOpenHandle "truncate" h $ \fd ->
    Posix.setFdSize fd (fromIntegral sz)

-- | Close handle
--
-- This is a no-op when the handle is already closed.
close :: FHandle -> IO ()
close h = closeHandleOS h Posix.closeFd

-- | File size of the given file pointer
--
-- NOTE: This is not thread safe (changes made to the file in other threads
-- may affect this thread).
getSize :: FHandle -> IO Word64
getSize h = withOpenHandle "getSize" h $ \fd ->
     fromIntegral . Posix.fileSize <$> Posix.getFdStatus fd

sameError :: FsError -> FsError -> Bool
sameError = sameFsError
