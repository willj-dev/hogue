{-|
Module      : Game.Hogue.Sector
Description : A sector of the map, one of nine in a 3x3 grid.
-}

module Game.Hogue.Sector where

import Data.Array (Ix)

data Sector = NW | N | NE | W | C | E | SW | S | SE
  deriving (Eq, Enum, Bounded, Show, Ord, Ix)

sectors :: [Sector]
sectors = enumFromTo minBound maxBound

sectorRow :: Sector -> Int
sectorRow NW  = 0
sectorRow N   = 0
sectorRow NE  = 0
sectorRow W   = 1
sectorRow C   = 1
sectorRow E   = 1
sectorRow SW  = 2
sectorRow S   = 2
sectorRow SE  = 2

sectorCol :: Sector -> Int
sectorCol NW  = 0
sectorCol W   = 0
sectorCol SW  = 0
sectorCol N   = 1
sectorCol C   = 1
sectorCol S   = 1
sectorCol NE  = 2
sectorCol E   = 2
sectorCol SE  = 2

adjacentSectors :: Sector -> [Sector]
adjacentSectors C   = [E, N, W, S]
adjacentSectors E   = [NE, C, SE]
adjacentSectors NE  = [N, E]
adjacentSectors N   = [NW, C, NE]
adjacentSectors NW  = [N, W]
adjacentSectors W   = [C, NW, SW]
adjacentSectors SW  = [S, W]
adjacentSectors S   = [SE, C, SW]
adjacentSectors SE  = [E, S]

-- | These constants define how much space is available for a room to take up. The map is made of 3x3 sectors, so the total size of the
--   map will thus be 81 columns by 24 rows. This is one character wider than the original Rogue (which was 80x24), because I wanted the
--   sectors to all be equal in size.
sectorWidth, sectorHeight, sectorCells, mapWidth, mapHeight, mapCells :: Int
sectorWidth = 27
sectorHeight = 8
sectorCells = sectorWidth * sectorHeight
mapWidth = sectorWidth * 3
mapHeight = sectorHeight * 3
mapCells = mapWidth * mapHeight
