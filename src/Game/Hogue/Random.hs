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

-- | Pass throughs to @System.Random@ which will be interpreted as a @State@ to carry the generator forward
data RogueRandom m a where
  Random :: R.Random r => RogueRandom m r
  RandomR :: R.Random r => (r, r) -> RogueRandom m r

makeSem ''RogueRandom

  -- | Produces a list with the given number of random values.
randomN :: (Member RogueRandom r, R.Random a) => Int -> Sem r [a]
randomN n = replicateM n random

-- | Produces a list with the given number of random values in the given range. Note that values may be repeated;
--   use 'take' with 'shuffle' to get distinct values from a list (assuming the list didn't have any duplicates
--   to begin with!)
randomRN :: (Member RogueRandom r, R.Random a) => (a, a) -> Int -> Sem r [a]
randomRN r n = replicateM n (randomR r)

-- | Generates a random number between 0 (inclusive) and the given positive integer (exclusive).
--   If the given number is less than or equal to 0, returns 0.
randomLT :: Member RogueRandom r => Int -> Sem r Int
randomLT x = randomR (0, x - 1)

-- | Flips a weighted coin. The given integer is a percent probability that the result will be true.
weightedFlip :: Member RogueRandom r => Int -> Sem r Bool
weightedFlip w = fmap (w <) (randomR (0, 99))

-- | Computes a "one in X" chance to be true.
oneIn :: Member RogueRandom r => Int -> Sem r Bool
oneIn x = fmap (== 0) (randomR (0, x - 1))

-- | Picks a random element from the given list. The list must be finite.
pick :: Member RogueRandom r => [a] -> Sem r a
pick xs = (xs !!) <$> randomR (0, length xs - 1)

shuffle :: Member RogueRandom r => [a] -> Sem r [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle xs = do
  i <- randomR (0, length xs - 2)
  let (head, x:tail) = splitAt i xs   -- no worries, the index must admit a nonempty list in snd
  (x :) <$> shuffle (head ++ tail)

-- | Transmutes RR actions into State actions to be handled later on
runRogueRandomAsState
  :: (RandomGen g, Member (State g) r)
  => Sem (RogueRandom ': r) a
  -> Sem r a
runRogueRandomAsState = interpret $ \case
  Random          -> bracketState R.random
  RandomR r       -> bracketState $ R.randomR r

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
