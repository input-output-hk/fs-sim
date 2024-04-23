{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.FS.Sim.Pure (
    PureSimFS
    -- opaque
  , pureHasFS
  , runPureSimFS
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Coerce (coerce)

import           System.FS.API

import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS (MockFS)
import qualified System.FS.Sim.Prim as Prim

-- | Monad useful for running 'HasFS' in pure code
newtype PureSimFS a = PureSimFS (Prim.FSSim a)
  deriving (Functor, Applicative, Monad, MonadState MockFS, MonadError FsError)

runPureSimFS :: PureSimFS a -> MockFS -> Either FsError (a, MockFS)
runPureSimFS (PureSimFS act) !st = Prim.runFSSim act st

pureHasFS :: HasFS PureSimFS Mock.HandleMock
pureHasFS = coerce Prim.pureHasFS
