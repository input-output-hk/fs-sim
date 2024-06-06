{-# OPTIONS_GHC -Wno-orphans #-}

module Test.System.FS.API.FsPath (tests) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (read)
import qualified System.FilePath as FilePath
import qualified System.FS.API as FS
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Test.System.FS.API.FsPath" [
      testProperty "prop_combineCommutes" prop_combineCommutes
    , testProperty "prop_addExtensionCommutes" prop_addExtensionCommutes
    ]

-- | Orphan instance that generates a __non-empty__ text!
instance Arbitrary Text where
  arbitrary = Text.pack <$> QC.listOf (QC.elements validChars) `suchThat` (not . null)
  shrink x = [ x''' | let x' = Text.unpack x
                    , x'' <- shrink x'
                    , not (null x'')
                    , let x''' = Text.pack x'' ]

-- | We pick a small subset of characters to use in directory/file names, so
-- that we don't break the invariant of 'FsPath'.
validChars :: [Char]
validChars = concat [['a'..'z'], ['A'..'Z'], ['0'..'9']]

-- | Commutativity property for 'FS.</>' and 'FilePath.</>'.
--
-- TODO: commutativity might not be the right name for this type of property.
--
-- @
--   \x y -> toFilePath (x </> y) == toFilePath x </> toFilePath y
-- @
--
-- The first argument is used to create a mount point, which makes the property
-- more useful because we are testing more cases. Also, for 'FS.fsToFilePath' to
-- work, we need at least the empty mountpoint.
prop_combineCommutes :: [Text] -> [Text] -> [Text] -> Property
prop_combineCommutes mnt path1 path2 =
      QC.classify (FilePath.isValid rhs) "Valid file path"
    $    lhs === rhs
    .&&. FilePath.makeValid lhs === FilePath.makeValid rhs
  where
    mnt' = filePathFromList mnt
    mnt'' = FS.MountPoint mnt'
    fsp = FS.fsPathFromList path1 FS.</> FS.fsPathFromList path2
    lhs = FS.fsToFilePath mnt'' fsp
    rhs = mnt' FilePath.</> filePathFromList path1 FilePath.</> filePathFromList path2

-- | Commutativity property for 'FS.<.>' and 'FilePath.<.>'.
--
-- TODO: commutativity might not be the right name for this type of property.
--
-- @
--   \path ext -> toFilePath (path <.> ext) == toFilePath path <.> ext
-- @
--
-- The first argument is used to create a mount point, which makes the property
-- more useful because we are testing more cases. Also, for 'FS.fsToFilePath' to
-- work, we need at least the empty mountpoint.
prop_addExtensionCommutes :: [Text] -> [Text] -> String -> Property
prop_addExtensionCommutes mnt path ext =
      QC.classify (FilePath.isValid rhs) "Valid file path"
    $ QC.classify (case ext of '.':_ -> True; _ -> False)
                  "Extension to add starts with an extension separator (.)"
    $    lhs === rhs
    .&&. FilePath.makeValid lhs === FilePath.makeValid rhs
  where
    mnt' = filePathFromList mnt
    mnt'' = FS.MountPoint mnt'
    fsp = FS.fsPathFromList path FS.<.> ext
    lhs = FS.fsToFilePath mnt'' fsp
    rhs = mnt' FilePath.</> filePathFromList path FilePath.<.> ext

-- | Build a 'FilePath' by 'FilePath.combine'ing the directory/file names.
filePathFromList :: [Text] -> FilePath
filePathFromList [] = []
filePathFromList xs = foldr (\y ys -> Text.unpack y FilePath.</> ys) (Text.unpack (last xs)) (init xs)
