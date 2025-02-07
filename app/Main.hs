module Main (main) where

import Game.Hogue.Level

import Control.Monad (sequence)
import System.Random (initStdGen)


main :: IO ()
main = do
  let lc = LevelConfig 1
  rng <- initStdGen
  lvl <- runGenerateLevel rng lc
  sequence (putStrLn <$> dbgMap lvl)
  return ()