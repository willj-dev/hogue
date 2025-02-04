{-|
Module      : Game.Hogue.Room
Description : A room in the dungeon: a rectangle, with doors. Note that items and monsters belong to the level itself, not the individual rooms.
-}

module Game.Hogue.Room (Room(..)) where

import Game.Hogue.Coord (Coord)

data Room = Room
  { nwCorner  :: Coord      -- ^ The coordinate of the wall at the top left corner of this room
  , seCorner  :: Coord      -- ^ The coordinate of the wall at the bottom right corner of this room
  , nDoor     :: Maybe Int  -- ^ If there is a door along the top wall of this room, it is this many spaces from the top left corner.
  , sDoor     :: Maybe Int  -- ^ Ditto, bottom wall, counting from bottom left corner.
  , wDoor     :: Maybe Int  -- ^ Ditto, left wall, counting from top left corner.
  , eDoor     :: Maybe Int  -- ^ Ditto, right wall, counting from top right corner.
  , isDark    :: Bool       -- ^ If this room is dark: the player can only see the contents of nearby spaces.
  }
