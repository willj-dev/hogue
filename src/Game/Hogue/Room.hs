{-|
Module      : Game.Hogue.Room
Description : A room in the dungeon, in three flavors.

Note that all coordinate values in a @Room@ are "local coordinates", measured relative to the top left corner of the sector
that this room is in.
-}

module Game.Hogue.Room
  ( Room(..)
  , Doors(..)
  , doors
  , floorCoords
  , hasFloor
  , noDoors
  , sectorWidth
  , sectorHeight
  ) where

import Game.Hogue.Coord
import Data.Array (range)
import Game.Hogue.Random (RogueRandom)
import Polysemy (Sem, Member)

data Room
  = BoxRoom   -- ^ An old-fashioned rectangular room with four walls and a floor
      Doors   -- ^ Where are the doors?
      Coord   -- ^ The coordinate of the wall in the top-left corner
      Coord   -- ^ The coordinate of the wall in the bottom-right corner
      Bool    -- ^ Whether or not this room is dark
  | MazeRoom  -- ^ A maze room, which is just a long winding corridor that fills a sector
      Doors   -- ^ Where are the entrances? These are always along the edge of the sector this maze belongs to
      [Coord] -- ^ Which tiles in this sector are corridors of the maze
  | GoneRoom  -- ^ Not a room at all, but just a space for corridors to move through
      Doors   -- ^ Locations of entrances along the edge of this sector
      Coord   -- ^ The point where corridors connecting to adjacent sectors join

instance Show Room where
  show (BoxRoom _ nwc@(nwx, nwy) (sex, sey) d) = (if d then "dark room " else "light room ") ++ "at " ++ show nwc ++ ", " ++ show (sex - nwx) ++ "x" ++ show (sey - nwy)
  show (MazeRoom _ _) = "maze room"
  show (GoneRoom _ c) = "gone room at " ++ show c

doors :: Room -> Doors
doors (BoxRoom d _ _ _) = d
doors (MazeRoom d _) = d
doors (GoneRoom d _) = d

-- | Does this room have valid floor space in which to put something?
hasFloor :: Room -> Bool
hasFloor (GoneRoom _ _) = False
hasFloor _ = True

-- | Returns the list of (local) coordinates at which something could be placed in this room
floorCoords :: Room -> [Coord]
floorCoords (BoxRoom _ nw se _) = range (nw .+. (1, 1), se .-. (1, 1))  -- anywhere except the walls around the edge
floorCoords (MazeRoom _ cs) = cs
floorCoords (GoneRoom _ _) = []

data Doors = Doors
  { nDoor     :: Maybe Int  -- ^ If there is a door along the top wall of this room, it is this many spaces from the top left corner.
  , sDoor     :: Maybe Int  -- ^ Ditto, bottom wall, counting from bottom left corner.
  , wDoor     :: Maybe Int  -- ^ Ditto, left wall, counting from top left corner.
  , eDoor     :: Maybe Int  -- ^ Ditto, right wall, counting from top right corner.
  }

noDoors :: Doors
noDoors = Doors Nothing Nothing Nothing Nothing

-- | These constants define how much space is available for a room to take up. The map is made of 3x3 sectors, so the total size of the
--   map will thus be 81 columns by 24 rows. This is one character wider than the original Rogue (which was 80x24), because I wanted the
--   sectors to all be equal in size.
sectorWidth, sectorHeight :: Int
sectorWidth = 27
sectorHeight = 8
