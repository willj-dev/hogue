{-|
Module      : Game.Hogue.State
Description : The main game state for Rogue, containing all the data necessary to render the map and react to the player's actions.
-}

module Game.Hogue.State (State(..)) where

import Game.Hogue.Daemon ( Daemon )
import Game.Hogue.Fuse ( Fuse )
import Game.Hogue.Level ( Level )
import Game.Hogue.Player ( Player )

data State rng = State
  { turn    :: Int      -- ^ How many turns have passed?
  , depth   :: Int      -- ^ How deep are we? (A positive number starting at 1)
  , pc      :: Player   -- ^ The adventurer and all of their goodies and stats
  , level   :: Level    -- ^ What is on the current level?
  , daemons :: [Daemon] -- ^ Ongoing effects / bookkeeping tasks
  , fuses   :: [Fuse]   -- ^ Ongoing timers for effects that expire
  , random  :: rng      -- ^ The current state of the game's RNG
  }
