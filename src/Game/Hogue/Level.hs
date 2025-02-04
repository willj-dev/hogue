{-|
Module      : Game.Hogue.Level
Description : The layout of the current level of the dungeon and everything in it.
-}

module Game.Hogue.Level (Level(..)) where

import Game.Hogue.Coord ( Coord )
import Game.Hogue.Corridor ( Corridor )
import Game.Hogue.Item ( Item )
import Game.Hogue.Monster ( Monster )
import Game.Hogue.Player ( Player )
import Game.Hogue.Room ( Room )

data Level = Level
  { depth     :: Int        -- ^ How deep are we? (A positive number starting at 1)
  , pc        :: Player     -- ^ The adventurer and all of their goodies and stats
  , rooms     :: [Room]     -- ^ The rooms on this level
  , corridors :: [Corridor] -- ^ The corridors connecting the rooms on this level
  , monsters  :: [Monster]  -- ^ The monsters in this level
  , items     :: [Item]     -- ^ The items on the floor in this level
  , exit      :: Coord      -- ^ The location of the stairs (up or down, depending on whether the player found the Amulet)
  }
