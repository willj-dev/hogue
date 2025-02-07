{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Game.Hogue.Log
Description : A small logging wrapper around stdout. TODO: this would be way better with printf style.
-}

module Game.Hogue.Log (Log, info, warn, runLogToStdout) where

import Polysemy

data Log m a where
  Info :: String -> Log m ()
  Warn :: String -> Log m ()
  -- errors get reported through a different effect

makeSem ''Log

runLogToStdout
  :: Member (Embed IO) r
  => Sem (Log : r) a
  -> Sem r a
runLogToStdout = interpret $ \case
  Info m -> embed (putStrLn $ "info: " ++ m)
  Warn m -> embed (putStrLn $ "warn: " ++ m)
