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

2. Connect the rooms with passages

    1. Pick a random room to start with.
    2. From the last connected room, randomly pick an adjacent room that isn't connected yet. If there are still unconnected rooms but none are adjacent (which
       might happen midway through this process), move randomly to a connected room which does have an unconnected neighbor.
    3. Create a passage connecting this room to the chosen adjacent room. Repeat from step 2 until all rooms are connected.
    4. Add up to 4 additional passages between rooms that don't already have passages between them.

3. Sprinkle some objects around the level, if the adventurer is returning with the amulet (instead of going deeper)

    1. There is a 5% chance per level for one of the box or maze rooms to be a treasure room: it has lots of items, but also lots of monsters.
    2. Try 9 times to put a random object in a box or maze room (36% chance each try).
    3. If the adventurer is at least on level 26, put the amulet in if they haven't found it already.

4. Put in traps. Starting with a 10% chance on level 1, 20% chance on level 2, etc; each level from 10 will always contain traps.

    1. Levels with traps have between 1 and 10 traps (deeper levels have an increasing chance of more traps)
    2. Find random empty spaces in box rooms to place the traps, picking a random trap type for each one

5. Put in the staircase: pick a random empty floor space in a maze or box room.

6. Put in the adventurer: pick a random empty floor space in a maze or box room.
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
