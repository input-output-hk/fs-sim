{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Possibly infinite streams of @'Maybe' a@s.
module System.FS.Sim.Stream (
    -- * Streams
    Stream
    -- * Running
  , runStream
    -- * Construction
  , always
  , empty
  , mkInfinite
  , repeating
  , unsafeMkFinite
    -- * Query
  , null
    -- * Generation and shrinking
  , genFinite
  , genInfinite
  , genMaybe
  , genMaybe'
  , shrinkStream
  ) where

import           Control.Monad (replicateM)
import           Prelude hiding (null)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  Streams
-------------------------------------------------------------------------------}

-- | A 'Stream' is a stream of @'Maybe' a@s, which is /possibly/ infinite or
-- /definitely/ finite.
--
-- Finiteness is tracked internally and used for 'QC.shrink'ing and the 'Show'
-- instance.
data Stream a = Stream {
      -- | Info about the size of the stream.
      _streamInternalInfo :: InternalInfo
    , _getStream          :: [Maybe a]
    }
  deriving Functor

-- | Tag for 'Stream's that describes whether it is either /definitely/ a finite
-- stream, or /possibly/ an infinite stream.
--
-- Useful for the 'Show' instance of 'Stream': when a 'Stream' is /definitely/
-- finite, we can safely print the full stream.
data InternalInfo = Infinite | Finite

-- | Fully shows a 'Stream' if it is /definitely/ finite, or prints a
-- placeholder string if it is /possibly/ infinite.
instance Show a => Show (Stream a) where
  showsPrec n (Stream info xs) = case info of
      Infinite -> ("<infinite stream>" ++)
      Finite   -> (if n > 10 then ('(':) else id)
                . shows xs
                . (" ++ ..." ++)
                . (if n > 10 then (')':) else id)

{-------------------------------------------------------------------------------
  Running
-------------------------------------------------------------------------------}

-- | Advance the 'Stream'. Return the @'Maybe' a@ and the remaining 'Stream'.
--
-- Returns 'Nothing' by default if the 'Stream' is empty.
runStream :: Stream a -> (Maybe a, Stream a)
runStream s@(Stream _    []    ) = (Nothing, s)
runStream   (Stream info (a:as)) = (a, Stream info as)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Make an empty 'Stream'.
empty :: Stream a
empty = Stream Finite []

-- | Make a 'Stream' that always generates the given @a@.
always :: a -> Stream a
always x = Stream Infinite (repeat (Just x))

-- | Make a 'Stream' that infinitely repeats the given list.
repeating :: [Maybe a] -> Stream a
repeating xs = Stream Infinite $ concat (repeat xs)

-- | UNSAFE: Make a 'Stream' that is marked as definitely finite.
--
-- This is unsafe since a user can pass in any list, and evaluating
-- 'Test.QuickCheck.shrink' or 'show' on the resulting 'Stream' will diverge. It
-- is the user's responsibility to only pass in a finite list.
unsafeMkFinite :: [Maybe a] -> Stream a
unsafeMkFinite = Stream Finite

-- | Make a 'Stream' that is marked as possibly infinite.
mkInfinite :: [Maybe a] -> Stream a
mkInfinite = Stream Infinite

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Return 'True' if the stream is empty.
--
-- A stream consisting of only 'Nothing's (even if it is only one) is not
-- considered to be empty.
null :: Stream a -> Bool
null (Stream _ []) = True
null _             = False

{-------------------------------------------------------------------------------
  Generation and shrinking
-------------------------------------------------------------------------------}

-- | Shrink a stream like it is an 'Test.QuickCheck.InfiniteList'.
--
-- Possibly infinite streams are shrunk differently than lists that are
-- definitely finite, which is to ensure that shrinking terminates.
-- * Possibly infinite streams are shrunk by taking finite prefixes of the
--  argument stream. As such, shrinking a possibly infinite stream creates
--  definitely finite streams.
-- * Definitely finite streams are shrunk like lists are shrunk normally,
--   preserving that the created streams are still definitely finite.
shrinkStream :: Stream a -> [Stream a]
shrinkStream (Stream info xs0) = case info of
    Infinite -> Stream Finite <$> [take n xs0 | n <- map (2^) [0 :: Int ..]]
    Finite   -> Stream Finite <$> QC.shrinkList (const []) xs0

-- | Make a @'Maybe' a@ generator based on an @a@ generator.
--
-- Each element has a chance of being either 'Nothing' or an element generated
-- with the given @a@ generator (wrapped in a 'Just').
--
-- The first argument is the likelihood (as used by 'QC.frequency') of a
-- 'Just' where 'Nothing' has likelihood 2.
genMaybe ::
     Int   -- ^ Likelihood of 'Nothing'
  -> Int   -- ^ Likelihood of @'Just' a@
  -> Gen a
  -> Gen (Maybe a)
genMaybe nLi jLi genA = QC.frequency
    [ (nLi, return Nothing)
    , (jLi, Just <$> genA)
    ]

-- | Like 'genMaybe', but with the likelihood of 'Nothing' fixed to @2@. 'QC.frequency'
genMaybe' ::
     Int   -- ^ Likelihood of @'Just' a@
  -> Gen a
  -> Gen (Maybe a)
genMaybe' = genMaybe 2

-- | Generate a finite 'Stream' of length @n@.
genFinite ::
     Int           -- ^ Requested size of finite stream. Tip: use 'genMaybe'.
  -> Gen (Maybe a)
  -> Gen (Stream a)
genFinite n gen = Stream Finite <$> replicateM n gen

-- | Generate an infinite 'Stream'.
genInfinite ::
     Gen (Maybe a)  -- ^ Tip: use 'genMaybe'.
  -> Gen (Stream a)
genInfinite gen = Stream Infinite <$> QC.infiniteListOf gen
