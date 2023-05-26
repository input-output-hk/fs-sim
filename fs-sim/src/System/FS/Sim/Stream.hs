{-# LANGUAGE DeriveFunctor #-}

module System.FS.Sim.Stream (
    Stream (..)
  , always
  , mkStream
  , mkStreamGen
  , null
  , runStream
  ) where

import           Control.Monad (replicateM)
import           Data.List (dropWhileEnd)
import           Data.Maybe (isNothing)
import           Prelude hiding (null)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  Streams
-------------------------------------------------------------------------------}

-- | A 'Stream' is a possibly infinite stream of @'Maybe' a@s.
newtype Stream a = Stream { getStream :: [Maybe a] }
    deriving (Show, Functor)

instance Semigroup (Stream a) where
  Stream s1 <> Stream s2 = Stream (zipWith pickLast s1 s2)
    where
      pickLast (Just x) Nothing = Just x
      pickLast _        mbY     = mbY

instance Monoid (Stream a) where
  mempty  = Stream (repeat Nothing)
  mappend = (<>)

-- | Create a 'Stream' based on the given possibly infinite list of @'Maybe'
-- a@s.
mkStream :: [Maybe a] -> Stream a
mkStream = Stream

-- | Advance the 'Stream'. Return the @'Maybe' a@ and the remaining 'Stream'.
runStream :: Stream a -> (Maybe a, Stream a)
runStream s@(Stream [])     = (Nothing, s)
runStream   (Stream (a:as)) = (a, Stream as)

-- | Make a 'Stream' that always generates the given @a@.
always :: a -> Stream a
always a = Stream (repeat (Just a))

-- | Make a 'Stream' generator based on a @a@ generator.
--
-- The generator generates a finite stream of 10 elements, where each element
-- has a chance of being either 'Nothing' or an element generated with the
-- given @a@ generator (wrapped in a 'Just').
--
-- The first argument is the likelihood (as used by 'QC.frequency') of a
-- 'Just' where 'Nothing' has likelihood 2.
mkStreamGen :: Int -> Gen a -> Gen (Stream a)
mkStreamGen justLikelihood genA =
    mkStream . dropWhileEnd isNothing <$> replicateM 10 mbGenA
  where
    mbGenA = QC.frequency
      [ (2, return Nothing)
      , (justLikelihood, Just <$> genA)
      ]

-- | Return 'True' if the stream is empty.
--
-- A stream consisting of only 'Nothing's (even if it is only one) is not
-- considered to be empty.
null :: Stream a -> Bool
null (Stream []) = True
null _           = False
