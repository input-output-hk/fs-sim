{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- The @base@ library throws warnings on uses of @head@ on later GHC versions.
-- This is a just a test module, so we ignore the warning.
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Test.System.FS.Sim.Stream (tests) where

import           Control.DeepSeq
import           Data.Maybe (isJust, isNothing)
import           Prelude hiding (filter, isInfinite, null)
import qualified Prelude
import           System.FS.Sim.Stream
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util

tests :: TestTree
tests = testGroup "Test.System.FS.Sim.Stream" [
      testProperty "prop_runStream" $
        prop_runStream @Int
    , testProperty "prop_runStreamN" $
        prop_runStreamN @Int
    , testProperty "prop_null"
        prop_null
    , testGroup "Shrinkers" [
          testProperty "prop_shrinkStreamFail" $
            prop_shrinkStreamFail @Int
        , testProperty "prop_eventuallyJust @InfiniteStream" $
            \(InfiniteStream s) -> prop_eventuallyJust @Int s
        , testProperty "prop_eventuallyNothing @InfiniteStream" $
            \(InfiniteStream s) -> prop_eventuallyNothing @Int s
        , testProperty "prop_eventuallyNothing @FiniteStream" $
            \(FiniteStream s) -> prop_eventuallyNothing @Int s
        ]
    , testGroup "Generators and shrinkers" [
          testGroup "Stream" $
            prop_forAllArbitraryAndShrinkSatisfy
              @(Stream Int)
              arbitrary
              (\s -> if isFinite s then shrink s else take 20 (shrink s))
              prop_stream
              prop_finiteStream
        , testGroup "FiniteSteam" $
            prop_arbitraryAndShrinkSatisfy
              @(FiniteStream Int)
              (\(FiniteStream s) -> prop_finiteStream s)
              (\(FiniteStream s) -> prop_finiteStream s)
        , testGroup "InfiniteStream" $
            prop_forAllArbitraryAndShrinkSatisfy
              @(InfiniteStream Int)
              arbitrary
              (take 6 . shrink)
              (\(InfiniteStream s) -> prop_infiniteStream s)
              (\(InfiniteStream s) -> prop_finiteStream s)
        , testGroup "NonEmptyStream" $
            prop_forAllArbitraryAndShrinkSatisfy
              @(NonEmptyStream Int)
              arbitrary
              (\s@(NonEmptyStream s') -> if isFinite s' then shrink s else take 20 (shrink s))
              (\(NonEmptyStream s) -> prop_nonEmptyStream s)
              (\(NonEmptyStream s) -> prop_nonEmptyStream s)
        ]
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Advancing a stream behaves like a head on an equivalent list.
prop_runStream :: (Eq a, Show a) => Stream a -> Property
prop_runStream s =
    x === head ys
  where
    (x, _s') = runStream s
    ys = runStreamIndefinitely s

-- | Advancing a stream @n@ times behaves likes @take n@ on an equivalent list.
prop_runStreamN :: (Eq a, Show a) => Int -> Stream a -> Property
prop_runStreamN n s =
    xs === take n ys
  where
    (xs, _s') = runStreamN n s
    ys = runStreamIndefinitely s

-- | Empty streams are null
prop_null :: Property
prop_null = once $ property $ null empty

{-------------------------------------------------------------------------------
  Shrinkers
-------------------------------------------------------------------------------}

-- | A simple property that is expected to fail, but should exercise the
-- shrinker a little bit.
prop_shrinkStreamFail :: Stream a -> Property
prop_shrinkStreamFail s = expectFailure $
    let xs = fst (runStreamN 10 s)
    in  property $ length (Prelude.filter isJust xs) /= length (Prelude.filter isNothing xs)

-- | A stream eventually produces a 'Just'
prop_eventuallyJust :: Stream a -> Property
prop_eventuallyJust = eventually isJust

-- | A stream eventually produces a 'Nothing'
prop_eventuallyNothing :: Stream a -> Property
prop_eventuallyNothing = eventually isNothing

eventually :: (Maybe a -> Bool) -> Stream a -> Property
eventually p = go 1
  where
    go !n s =
      let (x, s') = runStream s in
      if p x
        then tabulate "Number of elements inspected" [showPowersOf 2 n] $ property True
        else go (n+1) s'

{-------------------------------------------------------------------------------
  Generators and shrinkers
-------------------------------------------------------------------------------}

-- | A 'Stream' is either finite or infinite
prop_stream :: NFData a => Stream a -> Property
prop_stream s =
         prop_finiteStream s
    .||. prop_infiniteStream s

-- | A stream is finite
prop_finiteStream :: NFData a => Stream a -> Property
prop_finiteStream s =
    property (isFinite s) .&&. property (not (isInfinite s)) .&&.
    prop_deepseqStream s

-- | An stream is infinite
prop_infiniteStream :: Stream a -> Property
prop_infiniteStream s =
    property (isInfinite s) .&&. property (not (isFinite s))

-- | A stream is non-empty, and finite or infinite
prop_nonEmptyStream :: NFData a => Stream a -> Property
prop_nonEmptyStream s =
         property $ not (null s)
    .&&. prop_stream s

prop_deepseqStream :: NFData a => Stream a -> Property
prop_deepseqStream (UnsafeStream info xs) = property (rwhnf info `seq` rnf xs)

{-------------------------------------------------------------------------------
  Generators and shrinkers: utility properties
-------------------------------------------------------------------------------}

prop_arbitraryAndShrinkSatisfy ::
     forall a. (Arbitrary a, Show a)
  => (a -> Property) -- ^ Generator property
  -> (a -> Property) -- ^ Shrinker property
  -> [TestTree]
prop_arbitraryAndShrinkSatisfy =
    prop_forAllArbitraryAndShrinkSatisfy arbitrary shrink

prop_forAllArbitraryAndShrinkSatisfy ::
     forall a. Show a
  => Gen a
  -> (a -> [a])
  -> (a -> Property) -- ^ Generator property
  -> (a -> Property) -- ^ Shrinker property
  -> [TestTree]
prop_forAllArbitraryAndShrinkSatisfy gen shr pgen pshr =
    [ prop_forAllArbitrarySatisfies gen shr pgen
    , prop_forAllShrinkSatisfies gen shr pshr
    ]

prop_forAllArbitrarySatisfies ::
     forall a. Show a
  => Gen a
  -> (a -> [a])
  -> (a -> Property)
  -> TestTree
prop_forAllArbitrarySatisfies gen shr p =
    testProperty "Arbitrary satisfies property" $
      forAllShrink gen shr p

prop_forAllShrinkSatisfies ::
     forall a. Show a
  => Gen a
  -> (a -> [a])
  -> (a -> Property)
  -> TestTree
prop_forAllShrinkSatisfies gen shr p =
    testProperty "Shrinking satisfies property" $
      forAll gen $ \x ->
        case shr x of
          [] -> label "no shrinks" $ property True
          xs -> forAll (growingElements xs) p

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

--
-- Stream
--

instance Arbitrary a => Arbitrary (Stream a) where
  arbitrary = oneof [
        genFinite arbitrary
      , genInfinite arbitrary
      ]
  shrink = liftShrinkStream shrink

--
-- NonEmptyStream
--

newtype NonEmptyStream a = NonEmptyStream (Stream a)
  deriving stock Show

instance Arbitrary a => Arbitrary (NonEmptyStream a) where
  arbitrary = NonEmptyStream <$> oneof [
        genFiniteNonEmpty
      , genInfinite arbitrary
      ]
    where
      genFiniteNonEmpty = do
        x <- arbitrary
        xs <- arbitrary
        pure $ unsafeMkFinite (x : xs)
  shrink (NonEmptyStream s) =
      [ NonEmptyStream s'
      | s' <- shrink s
      , not (Prelude.null (unsafeStreamList s')) ]

--
-- FiniteStream
--

newtype FiniteStream a = FiniteStream {
    getFiniteStream :: Stream a
  }
  deriving stock Show

instance Arbitrary a => Arbitrary (FiniteStream a) where
  arbitrary = FiniteStream <$> genFinite arbitrary
  shrink (FiniteStream s) = FiniteStream <$> shrinkStream s

--
-- InfiniteStream
--

newtype InfiniteStream a = InfiniteStream {
    getInfiniteStream :: Stream a
  }
  deriving stock Show

instance Arbitrary a => Arbitrary (InfiniteStream a) where
  arbitrary = InfiniteStream <$> genInfinite arbitrary
  shrink (InfiniteStream s) = InfiniteStream <$> shrinkStream s

--
-- Tiny
--

newtype Tiny a = Tiny a
  deriving stock Show

instance Arbitrary (Tiny Int) where
  arbitrary = Tiny <$> choose (0, 5)
  shrink (Tiny x) = [ Tiny x' | x' <- shrink x, 0 <= x', x' <= 5]
