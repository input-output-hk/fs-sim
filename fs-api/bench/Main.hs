{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Control.DeepSeq (NFData (..))
import           Control.Exception (assert)
import           Control.Monad.Primitive (PrimMonad)
import           Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Int (Int64)
import           Data.List (unfoldr)
import           Data.Primitive.ByteArray
import           Data.Word (Word64)
import           Foreign (withForeignPtr)
import qualified GHC.Exts as GHC
import qualified GHC.ForeignPtr as GHC
import           GHC.Generics (Generic)
import qualified GHC.IO as GHC
import           GHC.Stack (HasCallStack)
import qualified System.Directory as Dir
import qualified System.FS.API as FS
import qualified System.FS.API.Lazy as FS
import           System.FS.IO (HandleIO, ioHasBufFS, ioHasFS)
import           System.FS.IO.Internal.Handle (HandleOS (..))
import           System.IO.Temp (createTempDirectory,
                     getCanonicalTemporaryDirectory)
import           System.Random (mkStdGen, uniform)

main :: IO ()
main = do
  putStrLn "WARNING: it is recommended to run each benchmark in isolation \
           \with short cooldown pauses in between benchmark executable \
           \invocations. This prevents noise coming from one benchmark \
           \from influencing another benchmark. Example incantion: \
           \cabal run fs-api-bench -- -m glob \"System.FS.API/hGetSome\""
  defaultMain [benchmarks]

benchmarks :: Benchmark
benchmarks = bgroup "System.FS.API" [
      envWithCleanup (mkFileEnv (4096 * 64) "hGetSome") cleanupFileEnv $ \ ~(hfs, _, _, fsp) ->
        bench "hGetSome" $
          perRunEnvWithCleanup (mkHandleEnv hfs fsp 0) (cleanupHandleEnv hfs) $ \h -> do
            FS.hGetSome hfs h (4096 * 64)
    , envWithCleanup (mkFileEnv (4096 * 64) "hGetSome'") cleanupFileEnv $ \ ~(hfs, hbfs, _, fsp) ->
        bench "hGetSome'" $
          perRunEnvWithCleanup (mkHandleEnv hfs fsp 0) (cleanupHandleEnv hfs) $ \h -> do
            hGetSome' hbfs h (4096 * 64)
    , envWithCleanup (mkFileEnv (4096 * 64) "hGetSomeAt") cleanupFileEnv $ \ ~(hfs, _, _, fsp) ->
        bench "hGetSomeAt" $
          perRunEnvWithCleanup (mkHandleEnv hfs fsp 0) (cleanupHandleEnv hfs) $ \h -> do
            FS.hGetSomeAt hfs h (4096 * 64) 0
    , envWithCleanup (mkFileEnv (4096 * 64) "hGetSomeAt'") cleanupFileEnv $ \ ~(hfs, hbfs, _, fsp) ->
        bench "hGetSomeAt'" $
          perRunEnvWithCleanup (mkHandleEnv hfs fsp 0) (cleanupHandleEnv hfs) $ \h -> do
            hGetSomeAt' hbfs h (4096 * 64) 0
    ]

{-------------------------------------------------------------------------------
  Benchmarkable functions
-------------------------------------------------------------------------------}

hGetSome' ::
     (HasCallStack, PrimMonad m)
  => FS.HasBufFS m h
  -> FS.Handle h
  -> Word64
  -> m BS.ByteString
hGetSome' hbfs !h !c = do
    !buf <- newPinnedByteArray (fromIntegral c)
    !c' <- FS.hGetBufSome hbfs h buf 0 (fromIntegral c)
    ba <- unsafeFreezeByteArray buf
    -- pure $ copyByteArrayToByteString ba 0 (fromIntegral c')
    pure $! unsafeByteArrayToByteString ba (fromIntegral c')

hGetSomeAt' ::
     (HasCallStack, PrimMonad m)
  => FS.HasBufFS m h
  -> FS.Handle h
  -> Word64
  -> FS.AbsOffset
  -> m BS.ByteString
hGetSomeAt' hbfs !h !c !off = do
    !buf <- newPinnedByteArray (fromIntegral c)
    !c' <- FS.hGetBufSomeAt hbfs h buf 0 (fromIntegral c) off
    ba <- unsafeFreezeByteArray buf
    -- pure $ copyByteArrayToByteString ba 0 (fromIntegral c')
    pure $! unsafeByteArrayToByteString ba (fromIntegral c')

{-# INLINE unsafeByteArrayToByteString #-}
unsafeByteArrayToByteString :: ByteArray -> Int -> BS.ByteString
unsafeByteArrayToByteString !ba !len =
    GHC.unsafeDupablePerformIO $ do
      let !(GHC.Ptr addr#) = byteArrayContents ba
      (MutableByteArray mba#) <- unsafeThawByteArray ba
      let fp = GHC.ForeignPtr addr# (GHC.PlainPtr mba#)
      pure $! BS.BS fp len

-- | Copy a 'Prim.ByteArray' at a certain offset and length into a
-- 'BS.ByteString'.
--
-- This is a copy of a function from @cborg@.
_copyByteArrayToByteString ::
     ByteArray -- ^ 'ByteArray' to copy from.
  -> Int -- ^ Offset into the 'ByteArray' to start with.
  -> Int -- ^ Length of the data to copy.
  -> BS.ByteString
_copyByteArrayToByteString ba off len =
    GHC.unsafeDupablePerformIO $ do
      fp <- BS.mallocByteString len
      withForeignPtr fp $ \ptr -> do
        copyByteArrayToPtr ptr ba off len
        return (BS.PS fp 0 len)

{-------------------------------------------------------------------------------
  Orphan instances
-------------------------------------------------------------------------------}

deriving stock instance Generic (HandleOS h)
deriving anyclass instance NFData (HandleOS h)
deriving anyclass instance NFData FS.FsPath
deriving anyclass instance NFData h => NFData (FS.Handle h)
instance NFData (FS.HasFS m h) where
  rnf hfs =
      dumpState `seq` hOpen `seq` hClose `seq` hIsOpen `seq` hSeek `seq`
      hGetSome `seq`hGetSomeAt `seq` hPutSome `seq` hTruncate `seq`
      hGetSize `seq` createDirectory `seq` createDirectoryIfMissing `seq`
      listDirectory `seq` doesDirectoryExist `seq` doesFileExist `seq`
      removeDirectoryRecursive `seq` removeFile `seq` renameFile `seq`
      mkFsErrorPath `seq` unsafeToFilePath `seq` ()
    where
      FS.HasFS {..} = hfs
      _coveredAllCases x = case x of
        FS.HasFS _a _b _c _d _e _f _g _h _i _j _k _l _m _n _o _p _q _r _s _t -> ()


instance NFData (FS.HasBufFS m h) where
  rnf hbfs = hPutBufSome `seq` hPutBufSomeAt `seq` ()
    where
      FS.HasBufFS { FS.hPutBufSome , FS.hPutBufSomeAt } = hbfs

{-------------------------------------------------------------------------------
  Environment initialisation and cleanup
-------------------------------------------------------------------------------}

mkFileEnv ::
     Int
  -> String
  -> IO (FS.HasFS IO HandleIO, FS.HasBufFS IO HandleIO, FilePath, FS.FsPath)
mkFileEnv nbytes dirName = do
    sysTmpDir <- getCanonicalTemporaryDirectory
    tmpDir <- createTempDirectory sysTmpDir dirName
    let hfs = ioHasFS (FS.MountPoint tmpDir)
        hbfs = ioHasBufFS (FS.MountPoint tmpDir)

    -- Create a file containing random bytes.
    let g = mkStdGen 17
        bytes = take nbytes $ unfoldr (Just . uniform) g
        bs = LBS.pack bytes
        fp = "benchfile"
        fsp = FS.mkFsPath [fp]
    FS.withFile hfs fsp (FS.WriteMode FS.MustBeNew) $ \h -> do
      nbytes' <- FS.hPutAll hfs h bs
      assert (nbytes == fromIntegral nbytes') $ pure ()

    -- Read the full file into memory to make doubly sure that the file is in
    -- the page cache, even though it might still be in the page cache as a
    -- result of writing the file.
    --
    -- Having the full file in the page cache will hopefully prevent some noise
    -- in the benchmark measurements.
    FS.withFile hfs fsp FS.ReadMode $ \h -> do
      bs' <- FS.hGetAll hfs h
      pure $! rnf bs'

    pure (hfs, hbfs, tmpDir, fsp)

cleanupFileEnv :: (a, b, FilePath, d) -> IO ()
cleanupFileEnv (_, _, fp, _) = Dir.removeDirectoryRecursive fp

mkHandleEnv :: FS.HasFS IO HandleIO -> FS.FsPath -> Int64 -> IO (FS.Handle HandleIO)
mkHandleEnv hfs fsp n = do
  h <- FS.hOpen hfs fsp FS.ReadMode
  FS.hSeek hfs h FS.AbsoluteSeek n
  pure h

cleanupHandleEnv :: FS.HasFS IO HandleIO -> FS.Handle HandleIO -> IO ()
cleanupHandleEnv = FS.hClose
