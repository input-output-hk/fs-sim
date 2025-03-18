{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use >=>" #-}

-- | 'HasFS' instance using 'MockFS' stored in an STM variable
module System.FS.Sim.STM (
    runSimFS
  , simHasFS
  , simHasFS'
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Primitive

import           System.FS.API

import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS (HandleMock, MockFS)
import           System.FS.Sim.Prim

{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

--- | Runs a computation provided an initial 'MockFS', producing a
--- result, the final state of the filesystem and a sequence of actions occurred
--- in the filesystem.
runSimFS :: (MonadSTM m, MonadCatch m, PrimMonad m)
         => MockFS
         -> (HasFS m HandleMock -> m a)
         -> m (a, MockFS)
runSimFS fs act = do
    var <- newTMVarIO fs
    a   <- act (simHasFS var)
    fs' <- atomically $ takeTMVar var
    return (a, fs')

-- | Alternative to 'simHasFS' that creates 'TVar's internally.
simHasFS' :: (MonadSTM m, MonadCatch m, PrimMonad m)
          => MockFS
          -> m (HasFS m HandleMock)
simHasFS' mockFS = simHasFS <$> newTMVarIO mockFS

-- | Equip @m@ with a @HasFs@ instance using the mock file system
simHasFS :: forall m. (MonadSTM m, MonadCatch m, PrimMonad m)
         => StrictTMVar m MockFS
         -> HasFS m HandleMock
simHasFS var = HasFS {
      dumpState                = sim     Mock.dumpState
    , hOpen                    = sim  .: Mock.hOpen
    , hClose                   = sim  .  Mock.hClose
    , hIsOpen                  = sim  .  Mock.hIsOpen
    , hSeek                    = sim ..: Mock.hSeek
    , hGetSome                 = sim  .: Mock.hGetSome
    , hGetSomeAt               = sim ..: Mock.hGetSomeAt
    , hPutSome                 = sim  .: Mock.hPutSome
    , hTruncate                = sim  .: Mock.hTruncate
    , hGetSize                 = sim  .  Mock.hGetSize
    , createDirectory          = sim  .  Mock.createDirectory
    , createDirectoryIfMissing = sim  .: Mock.createDirectoryIfMissing
    , listDirectory            = sim  .  Mock.listDirectory
    , doesDirectoryExist       = sim  .  Mock.doesDirectoryExist
    , doesFileExist            = sim  .  Mock.doesFileExist
    , removeDirectoryRecursive = sim  .  Mock.removeDirectoryRecursive
    , removeFile               = sim  .  Mock.removeFile
    , renameFile               = sim  .: Mock.renameFile
    , mkFsErrorPath            = fsToFsErrorPathUnmounted
    , unsafeToFilePath         = \_ -> error "simHasFS:unsafeToFilePath"
      -- File I\/O with user-supplied buffers
    , hGetBufSome              = sim ...:  Mock.hGetBufSome
    , hGetBufSomeAt            = sim ....: Mock.hGetBufSomeAt
    , hPutBufSome              = sim ...:  Mock.hPutBufSome
    , hPutBufSomeAt            = sim ....: Mock.hPutBufSomeAt
    }
  where
    sim :: FSSimT m a -> m a
    sim m = modifyTMVarIO var $ \st ->
        runFSSimT m st >>= \case
          Left e -> do
            throwIO e
          Right (a, st') -> do
            pure (a, st')

    (.:) :: (y -> z) -> (x0 -> x1 -> y) -> (x0 -> x1 -> z)
    (f .: g) x0 x1 = f (g x0 x1)

    (..:) :: (y -> z) -> (x0 -> x1 -> x2 -> y) -> (x0 -> x1 -> x2 -> z)
    (f ..: g) x0 x1 x2 = f (g x0 x1 x2)

    (...:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> y) -> (x0 -> x1 -> x2 -> x3 -> z)
    (f ...: g) x0 x1 x2 x3 = f (g x0 x1 x2 x3)

    (....:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> x4 -> y) -> (x0 -> x1 -> x2 -> x3 -> x4 -> z)
    (f ....: g) x0 x1 x2 x3 x4 = f (g x0 x1 x2 x3 x4)

modifyTMVarIO ::
     (MonadSTM m, MonadCatch m)
  => StrictTMVar m a -> (a -> m (b, a)) -> m b
modifyTMVarIO var k =
    fst . fst <$> generalBracket
      (atomically $ takeTMVar var)
      (\old -> \case
          ExitCaseSuccess (_, new)
            -> atomically $ putTMVar var new
          ExitCaseException _
            -> atomically $ putTMVar var old
          ExitCaseAbort
            -> atomically $ putTMVar var old
      )
      k

-- TODO: regression test
