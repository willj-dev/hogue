{-|
Module      : Game.Hogue.Room
Description : A room in the dungeon, in three flavors.
-}

module Game.Hogue.Room (Room(..), Doors(..), doors) where

import Game.Hogue.Coord (Coord)

data Room
  = BoxRoom   -- ^ An old-fashioned rectangular room with four walls and a floor
      Doors   -- ^ Where are the doors?
      Coord   -- ^ The location of the wall in the top-left corner
      Coord   -- ^ The location of the wall in the bottom-right corner
      Bool    -- ^ Whether or not this room is dark
  | MazeRoom  -- ^ A maze room, which is just a long winding corridor that fills a sector
      Doors   -- ^ Where are the entrances? These are always along the edge of the sector this maze belongs to
      [Coord] -- ^ Which tiles in this sector are corridors of the maze
  | GoneRoom  -- ^ Not a room at all, but just a space for corridors to move through
      Doors   -- ^ Locations of entrances along the edge of this sector
      Coord   -- ^ The point where corridors connecting to adjacent sectors join

doors :: Room -> Doors
doors (BoxRoom d _ _ _) = d
doors (MazeRoom d _) = d
doors (GoneRoom d _) = d

data Doors = Doors
  { nDoor     :: Maybe Int  -- ^ If there is a door along the top wall of this room, it is this many spaces from the top left corner.
  , sDoor     :: Maybe Int  -- ^ Ditto, bottom wall, counting from bottom left corner.
  , wDoor     :: Maybe Int  -- ^ Ditto, left wall, counting from top left corner.
  , eDoor     :: Maybe Int  -- ^ Ditto, right wall, counting from top right corner.
  }
