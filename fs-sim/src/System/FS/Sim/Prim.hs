{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

module System.FS.Sim.Prim (
    FSSimT
  , runFSSimT
  , FSSim
  , runFSSim
  , pureHasFS
  , primHasFS
  , primHasBufFS
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity (Identity (..))
import           Control.Monad.Primitive
import           Control.Monad.State

import           System.FS.API

import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS (MockFS)

newtype FSSimT m a = PureSimFS {
    unFSSimT :: StateT MockFS (ExceptT FsError m) a
  }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadState MockFS, MonadError FsError, PrimMonad )

runFSSimT :: FSSimT m a -> MockFS -> m (Either FsError (a, MockFS))
runFSSimT act !st = runExceptT $ flip runStateT st $ unFSSimT act

type FSSim = FSSimT Identity

runFSSim :: FSSim a -> MockFS -> Either FsError (a, MockFS)
runFSSim act !st = runIdentity $ runFSSimT act st

pureHasFS :: HasFS FSSim Mock.HandleMock
pureHasFS = HasFS {
      dumpState                = Mock.dumpState
    , hOpen                    = Mock.hOpen
    , hClose                   = Mock.hClose
    , hIsOpen                  = Mock.hIsOpen
    , hSeek                    = Mock.hSeek
    , hGetSome                 = Mock.hGetSome
    , hGetSomeAt               = Mock.hGetSomeAt
    , hPutSome                 = Mock.hPutSome
    , hTruncate                = Mock.hTruncate
    , hGetSize                 = Mock.hGetSize
    , createDirectory          = Mock.createDirectory
    , createDirectoryIfMissing = Mock.createDirectoryIfMissing
    , listDirectory            = Mock.listDirectory
    , doesDirectoryExist       = Mock.doesDirectoryExist
    , doesFileExist            = Mock.doesFileExist
    , removeDirectoryRecursive = Mock.removeDirectoryRecursive
    , removeFile               = Mock.removeFile
    , renameFile               = Mock.renameFile
    , mkFsErrorPath            = fsToFsErrorPathUnmounted
    , unsafeToFilePath         = \_ -> error "pureHasFS:unsafeToFilePath"
    }

primHasFS :: PrimMonad m => HasFS (FSSimT m) Mock.HandleMock
primHasFS = HasFS {
      dumpState                = Mock.dumpState
    , hOpen                    = Mock.hOpen
    , hClose                   = Mock.hClose
    , hIsOpen                  = Mock.hIsOpen
    , hSeek                    = Mock.hSeek
    , hGetSome                 = Mock.hGetSome
    , hGetSomeAt               = Mock.hGetSomeAt
    , hPutSome                 = Mock.hPutSome
    , hTruncate                = Mock.hTruncate
    , hGetSize                 = Mock.hGetSize
    , createDirectory          = Mock.createDirectory
    , createDirectoryIfMissing = Mock.createDirectoryIfMissing
    , listDirectory            = Mock.listDirectory
    , doesDirectoryExist       = Mock.doesDirectoryExist
    , doesFileExist            = Mock.doesFileExist
    , removeDirectoryRecursive = Mock.removeDirectoryRecursive
    , removeFile               = Mock.removeFile
    , renameFile               = Mock.renameFile
    , mkFsErrorPath            = fsToFsErrorPathUnmounted
    , unsafeToFilePath         = \_ -> error "pureHasFS:unsafeToFilePath"
    }

primHasBufFS :: PrimMonad m => HasBufFS (FSSimT m) Mock.HandleMock
primHasBufFS = HasBufFS {
      hGetBufSome   = Mock.hGetBufSome
    , hGetBufSomeAt = Mock.hGetBufSomeAt
    , hPutBufSome   = Mock.hPutBufSome
    , hPutBufSomeAt = Mock.hPutBufSomeAt
    }
