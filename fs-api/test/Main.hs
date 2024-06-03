module Main (main) where

import qualified Test.System.FS.API.FsPath
import qualified Test.System.FS.IO
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "fs-api-test" [
      Test.System.FS.API.FsPath.tests
    , Test.System.FS.IO.tests
    ]
