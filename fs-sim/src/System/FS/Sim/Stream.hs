{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Finite and infinite streams of @'Maybe' a@s.
module System.FS.Sim.Stream (
    -- * Streams
    Stream (..)
  , InternalInfo (..)
    -- * Running
  , runStream
  , runStreamN
  , runStreamIndefinitely
    -- * Construction
  , always
  , empty
  , repeating
  , unsafeMkInfinite
  , unsafeMkFinite
    -- * Modify
  , filter
    -- * Query
  , null
  , isFinite
  , isInfinite
    -- * Generation and shrinking
  , genFinite
  , genFiniteN
  , genInfinite
  , genMaybe
  , shrinkStream
  , liftShrinkStream
  ) where

import           Control.Monad (replicateM)
import           Prelude hiding (filter, isInfinite, null)
import qualified Prelude
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  Streams
-------------------------------------------------------------------------------}

-- | A stream of @'Maybe' a@s that can be infinite.
data Stream a =
  -- | UNSAFE: when constructing, modifying, or accessing the internals of a
  -- 'Stream', it is the responsibility of the user to preserve the following
  -- invariant:
  --
  -- INVARIANT: if the stream is marked as 'Infinite', then the internal list
  -- should be infinite. If the stream is marked as 'Finite', then the internal
  -- list should finite.
  --
  -- * If the internal list is infinite but marked as 'Finite', then 'QC.shrink'
  --   or 'show' on the corresponding stream will diverge.
  --
  -- * If the internal list is finite but marked as 'Infinite', then 'QC.shrink'
  --   on the corresponding stream will degrade to an infinite list of empty
  --   streams.
  UnsafeStream {
      -- | UNSAFE: see 'UnsafeStream' for more information.
      --
      -- Info about the finiteness of the stream. It is used for 'QC.shrink'ing
      -- and the 'Show' instance.
      unsafeStreamInternalInfo :: InternalInfo
      -- | UNSAFE: see 'UnsafeStream' for more information.
      --
      -- The internal list underlying the stream.
    , unsafeStreamList         :: [Maybe a]
    }
  deriving Functor

-- | Tag for 'Stream's that describes whether it is finite or infinite.
--
-- Useful for the 'Show' instance of 'Stream': when a 'Stream' is finite, we can
-- safely print the full stream.
data InternalInfo = Infinite | Finite

-- | Fully shows a 'Stream' if it is finite, or prints a placeholder string if
-- it is infinite.
instance Show a => Show (Stream a) where
  showsPrec n (UnsafeStream info xs) = case info of
      Infinite -> ("<infinite stream>" ++)
      Finite   -> (if n > 10 then ('(':) else id)
                . shows xs
                . (" ++ ..." ++)
                . (if n > 10 then (')':) else id)

{-------------------------------------------------------------------------------
  Running
-------------------------------------------------------------------------------}

-- | \( O(1) \): advance the 'Stream'. Return the @'Maybe' a@ and the remaining
-- 'Stream'.
--
-- Returns 'Nothing' by default if the 'Stream' is empty.
runStream :: Stream a -> (Maybe a, Stream a)
runStream s@(UnsafeStream _    []    ) = (Nothing, s)
runStream   (UnsafeStream info (a:as)) = (a, UnsafeStream info as)

-- | \( O(n) \): like 'runStream', but advancing the stream @n@ times.
--
-- If @n<=0@, then the stream is advanced @0@ times.
runStreamN :: Int -> Stream a -> ([Maybe a], Stream a)
runStreamN n s
  | n <= 0 = ([], s)
  | otherwise =
      let (x, s') = runStream s
          (xs, s'') = runStreamN (n-1) s'
      in  (x:xs, s'')

-- | \( O(\infty) \): like 'runStream', but advancing the stream indefinitely.
--
-- For infinite streams, this produces an infinite list. For finite streams,
-- this produces a finite list.
runStreamIndefinitely :: Stream a -> [Maybe a]
runStreamIndefinitely (UnsafeStream _ as) = as ++ repeat Nothing

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Make an empty 'Stream'.
empty :: Stream a
empty = UnsafeStream Finite []

-- | Make a 'Stream' that always generates the given @a@.
always :: a -> Stream a
always x = UnsafeStream Infinite (repeat (Just x))

-- | Make a 'Stream' that infinitely repeats the given list.
repeating :: [Maybe a] -> Stream a
repeating xs = UnsafeStream Infinite $ cycle xs

-- | UNSAFE: Make a 'Stream' that is marked as finite. It is the user's
-- responsibility to only pass in finite lists. See 'UnsafeStream' for more
-- information.
unsafeMkFinite :: [Maybe a] -> Stream a
unsafeMkFinite = UnsafeStream Finite

-- | UNSAFE: Make a 'Stream' that is marked as infinite. It is the user's
-- responsibility to only pass in infinite lists. See 'UnsafeStream' for more
-- information.
unsafeMkInfinite :: [Maybe a] -> Stream a
unsafeMkInfinite = UnsafeStream Infinite

{-------------------------------------------------------------------------------
  Modify
-------------------------------------------------------------------------------}

-- | Filter a 'Stream', preserving finiteness.
filter :: (Maybe a -> Bool) -> Stream a -> Stream a
filter p (UnsafeStream info xs) = UnsafeStream info (Prelude.filter p xs)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Check that the stream is empty.
--
-- In general, a stream is only empty if the stream is equivalent to 'empty'.
--
-- A finite\/infinite stream consisting of only 'Nothing's is not considered to
-- be empty. In particular, @'null' ('always' Nothing) /= True@.
null :: Stream a -> Bool
null (UnsafeStream Finite []) = True
null _                        = False

-- | Check that the stream is finite
isFinite :: Stream a -> Bool
isFinite (UnsafeStream Finite _)   = True
isFinite (UnsafeStream Infinite _) = False

-- | Check that the stream is infinite
isInfinite :: Stream a -> Bool
isInfinite (UnsafeStream Finite _)   = False
isInfinite (UnsafeStream Infinite _) = True

{-------------------------------------------------------------------------------
  Generation and shrinking
-------------------------------------------------------------------------------}

-- | Shrink a stream like it is an 'QC.InfiniteList'.
--
-- Infinite streams are shrunk differently than lists that are finite, which is
-- to ensure that we shrink infinite lists towards finite lists.
--
-- * Infinite streams are shrunk by taking finite prefixes of the argument
--   stream. Note that there are an infinite number of finite prefixes, so even
--   though the *shrink list* is infinite, the individual *list elements* are
--   finite.
--
-- * Finite streams are shrunk like lists are shrunk normally, preserving
--   finiteness.
shrinkStream :: Stream a -> [Stream a]
shrinkStream (UnsafeStream info xs0) = case info of
    Infinite -> UnsafeStream Finite <$> [take n xs0 | n <- map (2^) [0 :: Int ..]]
    Finite   -> UnsafeStream Finite <$> QC.shrinkList (const []) xs0

-- | Like 'shrinkStream', but with a custom shrinker for elements of the stream.
liftShrinkStream :: (Maybe a -> [Maybe a]) -> Stream a -> [Stream a]
liftShrinkStream shrinkOne (UnsafeStream info xs0) = case info of
    Infinite -> UnsafeStream Finite <$> [take n xs0 | n <- map (2^) [0 :: Int ..]]
    Finite   -> UnsafeStream Finite <$> QC.shrinkList shrinkOne xs0

-- | Make a @'Maybe' a@ generator based on an @a@ generator.
--
-- Each element has a chance of being either 'Nothing' or an element generated
-- with the given @a@ generator (wrapped in a 'Just'). These /likelihoods/ are
-- passed to 'QC.frequency'.
genMaybe ::
     Int -- ^ Likelihood of 'Nothing'
  -> Int -- ^ Likelihood of @'Just' a@
  -> Gen a
  -> Gen (Maybe a)
genMaybe nLi jLi genA = QC.frequency
    [ (nLi, return Nothing)
    , (jLi, Just <$> genA)
    ]

-- | Generate a finite 'Stream' of length @n@.
genFiniteN ::
     Int -- ^ Requested size of finite stream.
  -> Gen (Maybe a)
  -> Gen (Stream a)
genFiniteN n gen = UnsafeStream Finite <$> replicateM n gen

-- | Generate a sized, finite 'Stream'.
genFinite ::
     Gen (Maybe a)
  -> Gen (Stream a)
genFinite gen = UnsafeStream Finite <$> QC.listOf gen

-- | Generate an infinite 'Stream'.
genInfinite ::
     Gen (Maybe a)
  -> Gen (Stream a)
genInfinite gen = UnsafeStream Infinite <$> QC.infiniteListOf gen
