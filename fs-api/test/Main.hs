module Main (main) where

import           Test.System.FS.IO
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "fs-api-test" [
      Test.System.FS.IO.tests
    ]
