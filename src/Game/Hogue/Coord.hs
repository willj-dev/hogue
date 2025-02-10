{-|
Module      : Game.Hogue.Coord
Description : A data type for storing coordinates on the game's map, and tools for working with those values.
-}

module Game.Hogue.Coord where

import qualified Game.Hogue.Sector as S
import Game.Hogue.Sector (Sector, sectorCol, sectorRow, sectorWidth, sectorHeight, mapWidth, mapHeight)

data Coord = Coord
  { gx :: Int -- ^ What is the global x coordinate of this point?
  , gy :: Int -- ^ What is the global y coordinate of this point?
  } deriving (Eq)

instance Show Coord where
  show (Coord x y) = show (x, y)

instance Bounded Coord where
  minBound = Coord 0 0
  maxBound = Coord (mapWidth - 1) (mapHeight - 1)

instance Enum Coord where
  fromEnum (Coord x y) = mapWidth * y + x
  toEnum i = Coord (i `mod` mapWidth) (i `div` mapWidth)

-- | What sector is this point in?
sector :: Coord -> Sector
sector (Coord x y) = case (x `div` sectorWidth, y `div` sectorHeight) of
  (0, 0) -> S.NW
  (0, 1) -> S.W
  (0, 2) -> S.SW
  (1, 0) -> S.N
  (1, 1) -> S.C
  (1, 2) -> S.E
  (2, 0) -> S.SW
  (2, 1) -> S.S
  (2, 2) -> S.SE
  badSectorIndex -> error $ "GlobalCoord.localCoord: Invalid sector index: " ++ show badSectorIndex ++ " from global coord " ++ show (x,y)

-- | What is the local x coordinate of this point relative to its sector?
lx :: Coord -> Int
lx (Coord x _) = x `mod` sectorWidth

-- | What is the local y coordinate of this point relative to its sector?
ly :: Coord -> Int
ly (Coord _ y) = y `mod` sectorHeight

-- | Add a given offset to this coordinate. Returns Nothing if the result is out of bounds.
(.+.) :: Coord -> (Int, Int) -> Maybe Coord
(Coord x y) .+. (dx, dy) = let
  x' = x + dx
  y' = y + dy
  Coord maxX maxY = maxBound
  in if x' < 0 || x' >= maxX || y' < 0 || y' >= maxY then Nothing
  else Just $ Coord x' y'

-- | Computes the offset between two points.
(.-.) :: Coord -> Coord -> (Int, Int)
(Coord x' y') .-. (Coord x y) = (x' - x, y' - y)

-- | Create a Coord from sector-relative coordinates
local :: Sector -> Int -> Int -> Coord
local s x y = Coord (sectorCol s * sectorWidth + x) (sectorRow s * sectorHeight + y)

data Direction = E | N | W | S deriving (Eq, Show, Bounded, Enum)

allDirections :: [Direction]
allDirections = enumFromTo minBound maxBound

-- | Move n spaces in the specified direction. Returns Nothing if the resulting point would be out of bounds.
move :: Int -> Direction -> Coord -> Maybe Coord
move n E c = c .+. (n, 0)
move n N c = c .+. (0, -n)
move n W c = c .+. (-n, 0)
move n S c = c .+. (0, n)

-- | A list of all of the coordinates in the square formed with these two points at opposite corners, in reading order.
square :: Coord -> Coord -> [Coord]
square c1@(Coord x1 y1) c2@(Coord x2 y2)
  | y1 > y2 = square c2 c1  -- first make sure we start with the top side of the square...
  | x1 > x2 = square (Coord x2 y1) (Coord x1 y2)  -- and then make sure we start with the top left corner...
  | otherwise = [Coord x y | y <- [y1 .. y2], x <- [x1 .. x2]]
