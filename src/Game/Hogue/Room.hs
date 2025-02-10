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
  ) where

import Game.Hogue.Coord

data Room
  = BoxRoom   -- ^ An old-fashioned rectangular room with four walls and a floor
      Doors   -- ^ Where are the doors?
      Coord   -- ^ The local coordinate of the wall in the top-left corner
      Coord   -- ^ The local coordinate of the wall in the bottom-right corner
      Bool    -- ^ Whether or not this room is dark
  | MazeRoom  -- ^ A maze room, which is just a long winding corridor that fills a sector
      Doors   -- ^ Where are the entrances? These are always along the edge of the sector this maze belongs to
      [Coord] -- ^ Which tiles in this sector are corridors of the maze
  | GoneRoom  -- ^ Not a room at all, but just a space for corridors to move through
      Doors   -- ^ Locations of entrances along the edge of this sector
      Coord   -- ^ The point where corridors connecting to adjacent sectors join

instance Show Room where
  show (BoxRoom _ nw se d) =
    (if d then "dark room" else "light room")
    ++ " at " ++ show nw ++ ", "
    ++ show (dx + 1) ++ "x" ++ show (dy + 1)
    where (dx, dy) = se .-. nw
  show (MazeRoom _ maze) = "maze room (length: " ++ show (length maze) ++ ")"
  show (GoneRoom _ c) = "gone room at " ++ show c

doors :: Room -> Doors
doors (BoxRoom d _ _ _) = d
doors (MazeRoom d _) = d
doors (GoneRoom d _) = d

-- | Does this room have valid floor space in which to put something?
hasFloor :: Room -> Bool
hasFloor (GoneRoom _ _) = False
hasFloor _ = True

-- | Returns the list of coordinates at which something could be placed in this room
floorCoords :: Room -> [Coord]
floorCoords (BoxRoom _ nw se _) = let
  minX = gx nw + 1
  maxX = gx se - 1
  minY = gy nw + 1
  maxY = gy se - 1
  in square (Coord minX minY) (Coord maxX maxY)
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
