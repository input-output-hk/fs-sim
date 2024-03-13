{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Avoid lambda" -}

-- | 'HasFS' instance using 'MockFS' stored in an STM variable
module System.FS.Sim.STM (
    runSimFS
  , simHasFS
  , simHasFS'
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow

import           System.FS.API

import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS (HandleMock, MockFS)
import           System.FS.Sim.Pure (PureSimFS, runPureSimFS)

{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

--- | Runs a computation provided an initial 'MockFS', producing a
--- result, the final state of the filesystem and a sequence of actions occurred
--- in the filesystem.
runSimFS :: (MonadSTM m, MonadThrow m)
         => MockFS
         -> (HasFS m HandleMock -> m a)
         -> m (a, MockFS)
runSimFS fs act = do
    var <- newTVarIO fs
    a   <- act (simHasFS var)
    fs' <- readTVarIO var
    return (a, fs')

-- | Alternative to 'simHasFS' that creates 'TVar's internally.
simHasFS' :: (MonadSTM m, MonadThrow m)
          => MockFS
          -> m (HasFS m HandleMock)
simHasFS' mockFS = simHasFS <$> newTVarIO mockFS

-- | Equip @m@ with a @HasFs@ instance using the mock file system
simHasFS :: forall m. (MonadSTM m, MonadThrow m)
         => StrictTVar m MockFS
         -> HasFS m HandleMock
simHasFS var = HasFS {
      dumpState                = sim     Mock.dumpState
    , hOpen                    = \a b -> sim (Mock.hOpen a b)
    , hClose                   = \a -> sim (Mock.hClose a)
    , hIsOpen                  = \a -> sim (Mock.hIsOpen a)
    , hSeek                    = \a b c -> sim (Mock.hSeek a b c)
    , hGetSome                 = \a b ->  sim (Mock.hGetSome a b)
    , hGetSomeAt               = \a b c -> sim (Mock.hGetSomeAt a b c)
    , hPutSome                 = \a b -> sim (Mock.hPutSome a b)
    , hTruncate                = \a b -> sim (Mock.hTruncate a b)
    , hGetSize                 = \a -> sim  (Mock.hGetSize a)
    , createDirectory          = \a -> sim (Mock.createDirectory a)
    , createDirectoryIfMissing = \a b -> sim (Mock.createDirectoryIfMissing a b)
    , listDirectory            = \a -> sim (Mock.listDirectory a)
    , doesDirectoryExist       = \a -> sim (Mock.doesDirectoryExist a)
    , doesFileExist            = \a -> sim (Mock.doesFileExist a)
    , removeDirectoryRecursive = \a -> sim (Mock.removeDirectoryRecursive a)
    , removeFile               = \a -> sim (Mock.removeFile a)
    , renameFile               = \a b -> sim (Mock.renameFile a b)
    , mkFsErrorPath            = fsToFsErrorPathUnmounted
    , unsafeToFilePath         = \_ -> error "simHasFS:unsafeToFilePath"
    }
  where
    sim :: (forall s. PureSimFS s a) -> m a
    sim m = do
      eOrA <- atomically $ do
        st <- readTVar var
        case runPureSimFS m st of
          Left e -> return $ Left e
          Right (a, st') -> do
            writeTVar var st'
            return $ Right a
      either throwIO return eOrA
