{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Mocked, monad transformer-based implementation of the 'HasFS' interface.
module System.FS.Sim.Prim (
    FSSimT
  , runFSSimT
  , primHasMockFS
  ) where

import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.State

import           System.FS.API

import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS (MockFS)

-- | Monad transformer that extends a monad @m@ with pure features: (i) 'MockFS'
-- state, and (ii) throwing/catching 'FsError's.
newtype FSSimT m a = PureSimFS {
    unFSSimT :: StateT MockFS (ExceptT FsError m) a
  }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadState MockFS, MonadError FsError, PrimMonad )

runFSSimT :: FSSimT m a -> MockFS -> m (Either FsError (a, MockFS))
runFSSimT act !st = runExceptT $ flip runStateT st $ unFSSimT act

-- | Mocked, monad transformer-based implementation of the 'HasFS' interface.
--
-- This implementation is pure when running in a monad @m@ that is
-- 'Control.Monad.ST.ST'.
--
-- This implementation runs in a primitive monad @m@ extended with an 'FSSimT'
-- monad transformer. It is recommended to use 'System.FS.Sim.STM.simHasFS' or
-- 'System.FS.Sim.Error.mkSimErrorHasFS' instead because they hide the monad
-- transformer. The caveat is that @m@ should be IO-like.
primHasMockFS :: PrimMonad m => HasFS (FSSimT m) Mock.HandleMock
-- An alternative design could have fixed this implementation to
-- 'Control.Monad.ST.ST', and used 'Control.Monad.Class.MonadST.stToIO' to
-- convert between a pure and 'IO' version. However, it's simpler to just
-- overload this function.
primHasMockFS = HasFS {
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
      -- File I\/O with user-supplied buffers
    , hGetBufSome              = Mock.hGetBufSome
    , hGetBufSomeAt            = Mock.hGetBufSomeAt
    , hPutBufSome              = Mock.hPutBufSome
    , hPutBufSomeAt            = Mock.hPutBufSomeAt
    }
