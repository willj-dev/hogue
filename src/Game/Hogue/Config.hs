{-|
Module      : Game.Hogue.Config
Description : Various settings like probabilities governing level generation and the like
-}

module Game.Hogue.Config () where

data Config = Config
  { probabilities :: ProbabilityConfig
  }

data ProbabilityConfig = ProbabilityConfig
  { 
  }
