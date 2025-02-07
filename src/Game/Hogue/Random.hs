{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Game.Hogue.Random
Description : Helper effects for using and carrying along a RNG state.

Note that it seems new code is preferred to use @System.Random.Uniform@, but for some reason GHC was having trouble
with the instance for 'UniformRange (Int, Int)' even though it seems like it should be there.
-}

module Game.Hogue.Random where

import Control.Monad (replicateM)
import Polysemy
import Polysemy.State
import System.Random (RandomGen)
import qualified System.Random as R

data RogueRandom m a where
  -- | Pass through to standard @uniform@.
  Random :: R.Random r => RogueRandom m r
  -- | Produces a list with the given number of random values.
  RandomN :: R.Random r => Int -> RogueRandom m [r]
  -- | Pass through to standard @uniformR@.
  RandomR :: R.Random r => (r, r) -> RogueRandom m r
  -- | Produces a list with the given number of random values in the given range.
  RandomRN :: R.Random r => (r, r) -> Int -> RogueRandom m [r]
  -- | Picks a random element from the given list. The list must be finite.
  Pick :: [a] -> RogueRandom m a
  -- | Generates a random number between 0 (inclusive) and the given positive integer (exclusive).
  --   If the given number is less than or equal to 0, returns 0.
  RandomLT :: Int -> RogueRandom m Int
  -- | Flips a weighted coin. The given integer is a percent probability that the result will be true.
  WeightedFlip :: Int -> RogueRandom m Bool
  -- | Computes a "one in X" chance.
  OneIn :: Int -> RogueRandom m Bool

makeSem ''RogueRandom

-- | Transmutes RR actions into State actions to be handled later on
runRogueRandomAsState
  :: (RandomGen g, Member (State g) r)
  => Sem (RogueRandom ': r) a
  -> Sem r a
runRogueRandomAsState = interpret $ \case
  Random          -> bracketState R.random
  RandomN n       -> replicateM n $ bracketState R.random
  RandomR r       -> bracketState $ R.randomR r
  RandomRN r n    -> replicateM n $ bracketState $ R.randomR r
  Pick xs         -> bracketState $ \g -> let
    (i, g') = R.randomR (0, length xs - 1) g
    in (xs !! i, g')
  RandomLT ub     -> bracketState $ R.randomR (0, ub - 1)
  WeightedFlip w  -> fmap (w <) (bracketState $ R.randomR (0, 99))
  OneIn x         -> fmap (== 0) (bracketState $ R.randomR (0, x - 1))

-- | Given a function that takes in a state and outputs a result with a modified state, embeds this into the State monad
--   so that it automatically fetches the old state and stores the new state, then returns the result.
bracketState
  :: (Member (State s) r)
  => (s -> (a, s))
  -> Sem r a
bracketState f = do
  s <- get
  let (x, s') = f s
  put s'
  return x
