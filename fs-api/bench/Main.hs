{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Criterion.Main
import qualified Data.ByteString.Lazy as LBS
import           Data.Int
import           Data.List (unfoldr)
import           Debug.Trace
import           GHC.Generics
import qualified System.Directory as IO
import           System.Environment (getArgs)
import qualified System.FS.API as FS
import qualified System.FS.API.Lazy as FS
import           System.FS.IO
import           System.FS.IO.Internal.Handle as FS
import           System.IO.Temp
import           System.Random

main :: IO ()
main =
  -- _temp
  _bencher

_temp :: IO ()
_temp = do
  args <- getArgs
  before <- case args of
    []    -> error "Pass in True or False"
    [arg] -> pure (read arg)
    _     -> error "Pass in True or False"
  fileEnv@(hfs, _, _, fsp) <- mkFileEnv (4096 * 64) "_temp"
  let !() = rnf fileEnv
  traceMarkerIO "fileEnv"
  handleEnv@h <- mkHandleEnv hfs fsp 0
  let !() = rnf handleEnv
  traceMarkerIO "handlEnv"
  bs <- if before then
          FS.hGetSome_ hfs h 4096
        else
          FS.hGetSome hfs h 4096
  let !() = rnf bs
  traceMarkerIO "hGetSome"
  cleanupHandleEnv hfs h
  cleanupFileEnv fileEnv

_bencher :: IO ()
_bencher = defaultMain [benchmarks]

benchmarks :: Benchmark
benchmarks = bgroup "System.FS.API" [
      envWithCleanup (mkFileEnv (4096 * 64) "hGetSome-after") cleanupFileEnv $ \ ~(hfs, _, _, fsp) ->
        bench "hGetSome after" $
          perRunEnvWithCleanup (mkHandleEnv hfs fsp 0) (cleanupHandleEnv hfs) $ \h -> do
            FS.hGetSome hfs h (4096 * 64)
    , envWithCleanup (mkFileEnv (4096 * 64) "hGetSome-before") cleanupFileEnv $ \ ~(hfs, _, _, fsp) ->
        bench "hGetSome before" $
          perRunEnvWithCleanup (mkHandleEnv hfs fsp 0) (cleanupHandleEnv hfs) $ \h -> do
            FS.hGetSome_ hfs h (4096 * 64)
    ]

deriving stock instance Generic (FS.HandleOS h)
deriving anyclass instance NFData (FS.HandleOS h)
deriving anyclass instance NFData FS.FsPath
deriving anyclass instance NFData h => NFData (FS.Handle h)
instance NFData (FS.HasFS m h) where
  rnf hfs =
      dumpState `seq` hOpen `seq` hClose `seq` hIsOpen `seq` hSeek `seq`
      hGetSome_ `seq` hGetBufSome `seq` hGetSomeAt_ `seq` hGetBufSomeAt `seq`
      hPutSome `seq` hTruncate `seq`
      hGetSize `seq` createDirectory `seq` createDirectoryIfMissing `seq`
      listDirectory `seq` doesDirectoryExist `seq` doesFileExist `seq`
      removeDirectoryRecursive `seq` removeFile `seq` renameFile `seq`
      mkFsErrorPath `seq` unsafeToFilePath `seq` ()
    where
      FS.HasFS {..} = hfs
      _coveredAllCases x = case x of
        FS.HasFS _a _b _c _d _e _f _g _h _i _j _k _l _m _n _o _p _q _r _s _t _u _v -> ()


instance NFData (FS.HasBufFS m h) where
  rnf hbfs = hPutBufSome `seq` hPutBufSomeAt `seq` ()
    where
      FS.HasBufFS { FS.hPutBufSome , FS.hPutBufSomeAt } = hbfs


mkHandleEnv :: FS.HasFS IO HandleIO -> FS.FsPath -> Int64 -> IO (FS.Handle HandleIO)
mkHandleEnv hfs fsp n = do
  h <- FS.hOpen hfs fsp FS.ReadMode
  FS.hSeek hfs h FS.AbsoluteSeek n
  pure h

cleanupHandleEnv :: FS.HasFS IO HandleIO -> FS.Handle HandleIO -> IO ()
cleanupHandleEnv = FS.hClose

mkFileEnv :: Int -> String -> IO (FS.HasFS IO HandleIO, FS.HasBufFS IO HandleIO, FilePath, FS.FsPath)
mkFileEnv nbytes dirName = do
    sysTmpDir <- getCanonicalTemporaryDirectory
    tmpDir <- createTempDirectory sysTmpDir dirName
    let hfs = ioHasFS (FS.MountPoint tmpDir)
        hbfs = ioHasBufFS (FS.MountPoint tmpDir)

    let g = mkStdGen 17
        bytes = take nbytes $ unfoldr (Just . uniform) g
        bs = LBS.pack bytes

    let fp = "benchfile"
        fsp = FS.mkFsPath [fp]
    FS.withFile hfs fsp (FS.WriteMode FS.MustBeNew) $ \h -> do
      nbytes' <- FS.hPutAll hfs h bs
      assert (nbytes == fromIntegral nbytes') $ pure ()

    FS.withFile hfs fsp FS.ReadMode $ \h -> do
      bs' <- FS.hGetAll hfs h
      let !() = rnf bs'
      pure ()

    pure (hfs, hbfs, tmpDir, fsp)

cleanupFileEnv :: (a, b, FilePath, d) -> IO ()
cleanupFileEnv (_, _, fp, _) = IO.removeDirectoryRecursive fp
