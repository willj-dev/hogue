{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Game.Hogue.Level
Description : The layout of the current level of the dungeon and everything in it.

Process for generating a new level:

1. Build the rooms

    1. A level is divided into 9 sectors (3 x 3), each of which can have one of three types of rooms:

        * A box room (the default), which could be anywhere from a 4x4 square up to the full sector area.
        * A maze room (tiny chance at first, growing to a 1 in 15 chance past level 10), which is a long and winding corridor that fills the whole sector
        * A "gone" room (up to 3 per level), which can just support a passage between adjacent rooms (or a dead end from one of them)

    2. After deciding on the shape of each room, put a pile of gold somewhere (50% chance for each box or maze room, and only if the adventurer
       does not have the amulet or is going even deeper into the dungeon)
    3. Possibly put a monster into each box or maze room. If there is gold in the room, 80% chance; otherwise, 25% chance.

2. Put in the staircase: pick a random empty floor space in a maze or box room. Note that this occurs at the end of the process in the original Rogue,
   but because I never want it to be possible to have a nonsensical @Level@ value, we'll find a spot for this immediately once we have the rooms themselves.

3. Connect the rooms with passages

    1. Pick a random room to start with.
    2. From the last connected room, randomly pick an adjacent room that isn't connected yet. If there are still unconnected rooms but none are adjacent (which
       might happen midway through this process), move randomly to a connected room which does have an unconnected neighbor.
    3. Create a passage connecting this room to the chosen adjacent room. Repeat from step 2 until all rooms are connected.
    4. Add up to 4 additional passages between rooms that don't already have passages between them.

4. Sprinkle some objects around the level, if the adventurer is returning with the amulet (instead of going deeper)

    1. There is a 5% chance per level for one of the box or maze rooms to be a treasure room: it has lots of items, but also lots of monsters.
    2. Try 9 times to put a random object in a box or maze room (36% chance each try).
    3. If the adventurer is at least on level 26, put the amulet in if they haven't found it already.

5. Put in traps. Starting with a 10% chance on level 1, 20% chance on level 2, etc; each level from 10 will always contain traps.

    1. Levels with traps have between 1 and 10 traps (deeper levels have an increasing chance of more traps)
    2. Find random empty spaces in box rooms to place the traps, picking a random trap type for each one
-}

module Game.Hogue.Level where

import Control.Lens hiding (Level, indices)
import Polysemy
import Polysemy.Reader
import Polysemy.State

import Game.Hogue.Coord
import Game.Hogue.Corridor ( Corridor )
import Game.Hogue.Item ( Item )
import Game.Hogue.Log
import Game.Hogue.Monster ( Monster )
import Game.Hogue.Random
import Game.Hogue.Room
import Data.Array (range, Array, assocs, (!), array, indices)
import Control.Monad (replicateM)
import System.Random (RandomGen)

data LevelConfig = LevelConfig
  { _depth :: Int
  }

makeLenses 'LevelConfig

type Rooms = Array Coord Room

data Level = Level
  { _rooms     :: Rooms      -- ^ The rooms on this level
  , _exit      :: Coord      -- ^ The location of the stairs (up or down, depending on whether the player found the Amulet)
  , _corridors :: [Corridor] -- ^ The corridors connecting the rooms on this level
  , _monsters  :: [Monster]  -- ^ The monsters in this level
  , _items     :: [Item]     -- ^ The items on the floor in this level
  }

makeLenses 'Level

runGenerateLevel :: RandomGen g => g -> LevelConfig -> IO Level
runGenerateLevel rng lc
  = generateLevel lc
  & runRogueRandomAsState
  & evalState rng
  & runLogToStdout
  & runM

generateLevel
  :: Members [RogueRandom, Log] r
  => LevelConfig
  -> Sem r Level
generateLevel c = runReader c $ do
  emptyRooms <- makeRooms
  (l, _) <- runState emptyRooms $ connectRooms
    >> addItems
    >> addTraps
  return l

-- | First things first: what are the shapes of the rooms?
makeRooms
  :: Members [Reader LevelConfig, RogueRandom, Log] r
  => Sem r Level
makeRooms = do
  nGoneRooms <- randomLT 4
  goneRoomIndices <- take nGoneRooms <$> shuffle sectorIndices
  let hereRoomIndices = filter (`notElem` goneRoomIndices) sectorIndices
  goneRooms <- zip goneRoomIndices <$> replicateM nGoneRooms newGoneRoom
  info $ "gone rooms: " ++ show goneRooms
  hereRooms <- zip hereRoomIndices <$> replicateM (9 - nGoneRooms) newHereRoom
  info $ "here rooms: " ++ show hereRooms
  emptyLevel $ array sectorIndexRange (goneRooms ++ hereRooms)

-- | This makes a level with empty rooms and no corridors between them. Because we always want to be able to index the rooms array,
--   the rooms are required first, but everything else is okay to leave empty while we continue the process of constructing the level.
--   We also figure out where the stairs should go here; using a placeholder value would make an invalid state, which we are trying to avoid!
emptyLevel :: Member RogueRandom r => Rooms -> Sem r Level
emptyLevel rs = do
  stairCoord <- pickFloor rs
  return $ Level rs stairCoord [] [] []

-- | Make a new box or maze rooom, without doors for now
newHereRoom :: Members [Reader LevelConfig, RogueRandom] r => Sem r Room
newHereRoom = do
  d <- asks (^. depth)
  makeDark <- (< d - 1) <$> randomLT 10
  makeMaze <- (makeDark &&) <$> oneIn 15
  if makeMaze then newMazeRoom else newBoxRoom makeDark

-- | Make a new box room, placed randomly in its sector and of a random size (at least 2x2 floor spaces),
--   with at least one space of padding around the edge for passages. No doors for now.
newBoxRoom :: Member RogueRandom r => Bool -> Sem r Room
newBoxRoom isDark = do
  roomWidth <- randomR (4, sectorWidth - 2)
  roomHeight <- randomR (4, sectorHeight - 2)
  roomX <- randomR (1, sectorWidth - roomWidth - 1)
  roomY <- randomR (1, sectorHeight - roomHeight - 1)
  return $ BoxRoom noDoors (roomX, roomY) (roomX + roomWidth - 1, roomY + roomHeight - 1) isDark

-- | TODO
newMazeRoom :: Member RogueRandom r => Sem r Room
newMazeRoom = do
  firstCoord <- randomR ((0, 0), (sectorWidth - 1, sectorHeight - 1))
  mazeCoords <- continueMaze [firstCoord]
  return $ MazeRoom noDoors mazeCoords

continueMaze :: Member RogueRandom r => [Coord] -> Sem r [Coord]
continueMaze maze = case allowedMoves maze of
  [] -> return maze
  moves -> do
    nextMove <- pick moves
    continueMaze (nextMove ++ maze)

-- | To build a maze, we check 2 spaces in each direction from where we are now. (We want walls between )
allowedMoves :: [Coord] -> [[Coord]]
allowedMoves [] = [] -- this really shouldn't happen but GHC was yelling at me
allowedMoves (pos : prev) = let
  allDirections = [E, N, W, S]
  canMove d = let target = move pos d 2 in validLocalCoord target && target `notElem` prev
  allowedDirections = filter canMove allDirections
  in (\d -> [move pos d 2, move pos d 1]) <$> allowedDirections

-- | Pick a random spot to connect adjacent corridors through in this sector. No doors for now.
newGoneRoom :: Member RogueRandom r => Sem r Room
newGoneRoom = do
  crossX <- randomR (1, sectorWidth - 2)  -- leave one space of padding for passages
  crossY <- randomR (1, sectorHeight - 2)
  return $ GoneRoom noDoors (crossX, crossY)

connectRooms
  :: Members [Reader LevelConfig, RogueRandom, State Level] r
  => Sem r ()
connectRooms = return ()

addItems
  :: Members [Reader LevelConfig, RogueRandom, State Level] r
  => Sem r ()
addItems = return ()

addTraps
  :: Members [Reader LevelConfig, RogueRandom, State Level] r
  => Sem r ()
addTraps = return ()

-- | The range of sector indices (inclusive)
sectorIndexRange :: (Coord, Coord)
sectorIndexRange = ((0, 0), (2, 2))

-- | All of the valid room indices from top left (0,0) to bottom right (2,2)
sectorIndices :: [Coord]
sectorIndices = range sectorIndexRange

-- | The range of local coordinates (inclusive)
localCoordRange :: (Coord, Coord)
localCoordRange = ((0, 0), (sectorWidth - 1, sectorHeight - 1))

-- | All of the global coordinates, in reading order (row by row)
globalCoordReadingOrder :: [Coord]
globalCoordReadingOrder = [(x, y) | y <- [0 .. 3 * sectorHeight - 1], x <- [0 .. 3 * sectorWidth - 1]]

-- | Returns the room index of a random box or maze room.
pickRoomWithFloor :: Member RogueRandom r => Rooms -> Sem r Coord
pickRoomWithFloor rs = (^._1) <$> pick (filter (hasFloor . snd) $ assocs rs)

-- | Pick a random floor tile from among the rooms that have them.
pickFloor :: Member RogueRandom r => Rooms -> Sem r Coord
pickFloor rs = do
  roomIndex <- pickRoomWithFloor rs
  lc <- pick (floorCoords $ rs ! roomIndex)
  return $ mapCoord roomIndex lc

-- | Compute the global map coordinate from the given sector index and sector coordinate.
mapCoord :: Coord -> Coord -> Coord
mapCoord (six, siy) (sx, sy) = (six * sectorWidth + sx, siy * sectorHeight + sy)

-- | Compute the sector index and local coordinate from a global coordinate
localCoord :: Coord -> (Coord, Coord)
localCoord (x, y) = ((x `div` sectorWidth, y `div` sectorHeight), (x `mod` sectorWidth, y `mod` sectorHeight))

validLocalCoord :: Coord -> Bool
validLocalCoord (x, y)
  | x < 0             = False
  | x >= sectorWidth  = False
  | y < 0             = False
  | y >= sectorHeight = False
  | otherwise         = True

dbgCharAt :: Level -> Coord -> Char
dbgCharAt l c = let
  (si, sc@(sx, sy)) = localCoord c
  room = (l ^. rooms) ! si
  in if c == (l ^. exit) then '%' else case room of
    BoxRoom _ (nwx, nwy) (sex, sey) d ->
      if sy < nwy || sx < nwx || sy > sey || sx > sex then ' '
      else if sy == nwy || sy == sey then '='
      else if sx == nwx || sx == sex then '|'
      else if d then '*'
      else '.'
    MazeRoom _ coords -> if sc `elem` coords then '#' else ' '
    GoneRoom _ gc -> if gc == sc then 'X' else ' '

dbgMap :: Level -> [String]
dbgMap l = [ [ dbgCharAt l (x, y) | x <- [0 .. 3 * sectorWidth - 1] ] | y <- [0 .. 3 * sectorHeight - 1] ]
