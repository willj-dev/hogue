module Main (main) where

import Game.Hogue.Level

import Control.Monad (sequence, replicateM_)
import System.Random (initStdGen)

dbgLvl :: Int -> IO ()
dbgLvl d = do
  let lc = LevelConfig d
  rng <- initStdGen
  lvl <- runGenerateLevel rng lc
  mapM_ putStrLn (dbgMap lvl)

main :: IO ()
main = replicateM_ 25 (dbgLvl 15)