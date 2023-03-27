{-# LANGUAGE CPP #-}

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
            -- TODO: The FS tests fail for darwin on CI, see #532. So, they are
            -- disabled for now, but should be enabled once #532 is resolved.
            testGroup "FS" $
              [ Test.System.FS.StateMachine.tests tmpDir | not darwin] <>
              [ Test.System.FS.Sim.FsTree.tests
              ]
          ]
      ]

darwin :: Bool
#ifdef darwin_HOST_OS
darwin = True
#else
darwin = False
#endif
