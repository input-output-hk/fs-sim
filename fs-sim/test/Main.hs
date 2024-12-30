module Main (main) where

import qualified Test.System.FS.Sim.Error
import qualified Test.System.FS.Sim.FsTree
import qualified Test.System.FS.Sim.Stream
import qualified Test.System.FS.StateMachine
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "fs-sim-test" [
      Test.System.FS.Sim.Error.tests
    , Test.System.FS.Sim.FsTree.tests
    , Test.System.FS.Sim.Stream.tests
    , Test.System.FS.StateMachine.tests
    ]
