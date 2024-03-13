{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module System.FS.Sim.Pure (
    PureSimFS
    -- opaque
  , pureHasFS
  , runPureSimFS
    -- * HasBufFS
  , pureHasBufFS
  ) where

import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State

import           System.FS.API

import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS (MockFS)

-- | Monad useful for running 'HasFS' in pure code
newtype PureSimFS s a = PureSimFS (StateT MockFS (ExceptT FsError (ST s)) a)
  deriving (Functor)

unPureSimFS :: PureSimFS s a -> StateT MockFS (ExceptT FsError (ST s)) a
unPureSimFS (PureSimFS act) = act

instance Applicative (PureSimFS s) where
  pure :: a -> PureSimFS s a
  pure x = PureSimFS $ pure x

  (<*>) :: PureSimFS s (a -> b) -> PureSimFS s a -> PureSimFS s b
  f <*> x = PureSimFS (unPureSimFS f <*> unPureSimFS x)

instance Monad (PureSimFS s) where
  (>>=) :: PureSimFS s a -> (a -> PureSimFS s b) -> PureSimFS s b
  x >>= f = PureSimFS $ unPureSimFS x >>= unPureSimFS . f

instance MonadState MockFS (PureSimFS s) where
  state :: (MockFS -> (a, MockFS)) -> PureSimFS s a
  state = PureSimFS . state

instance MonadError FsError (PureSimFS s) where
  throwError :: FsError -> PureSimFS s a
  throwError = PureSimFS . throwError

  catchError :: PureSimFS s a -> (FsError -> PureSimFS s a) -> PureSimFS s a
  catchError act catch = PureSimFS $ catchError (unPureSimFS act) (unPureSimFS . catch)

instance PrimMonad (PureSimFS s) where
  type PrimState (PureSimFS s) = s
  primitive act = PureSimFS $ primitive act

runPureSimFS :: (forall s. PureSimFS s a) -> MockFS -> Either FsError (a, MockFS)
runPureSimFS act !st = runST $ runExceptT $ flip runStateT st $ unPureSimFS act

pureHasFS :: HasFS (PureSimFS s) Mock.HandleMock
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

pureHasBufFS :: HasBufFS (PureSimFS s) Mock.HandleMock
pureHasBufFS = HasBufFS {
      hGetBufSome   = Mock.hGetBufSome
    , hGetBufSomeAt = Mock.hGetBufSomeAt
    , hPutBufSome   = Mock.hPutBufSome
    , hPutBufSomeAt = Mock.hPutBufSomeAt
    }

