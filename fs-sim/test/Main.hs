module Main (main) where

import           System.IO.Temp (withSystemTempDirectory)

import           Test.Tasty

import qualified Test.System.FS.Sim.FsTree
import qualified Test.System.FS.StateMachine

main :: IO ()
main = withSystemTempDirectory "fs-sim-test" $ \tmpDir ->
  defaultMain $
    testGroup "Test" [
        testGroup "System" [
            testGroup "FS" [
                Test.System.FS.StateMachine.tests tmpDir
              , Test.System.FS.Sim.FsTree.tests
              ]
          ]
      ]
