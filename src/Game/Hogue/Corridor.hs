{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Game.Hogue.Corridor
Description : A data type representing a corridor between rooms in the dungeon, connecting one door to another.
-}

module Game.Hogue.Corridor where

import Polysemy
import Control.Lens

import Game.Hogue.Coord
import Game.Hogue.Random

newtype Corridor = Corridor { _path :: [Coord] }

instance Semigroup Corridor where
  Corridor p1 <> Corridor p2 = Corridor (p1 ++ p2)

instance Monoid Corridor where
  mempty = Corridor []

(:-) :: Coord -> Corridor -> Corridor
c :- Corridor path = Corridor (c : path)

makeLenses 'Corridor

-- | Makes a corridor between two points, NOT including either the start or end point.
corridor :: Member HogueRandom r =>
  Coord -> Coord -> Sem r Corridor
corridor from to
  | sector from > sector to = corridor to from -- Only draw to the right or down
  | succ (sector from) == sector to = do -- moving right
    let (dx, dy) = to .-. from
    turnAt <- randomR (1, dx - 1)
    
  | otherwise = undefined -- moving down

pathRight :: Int -> Coord -> Corridor
pathRight n _ | n <= 0 = mempty
pathRight n c@(Coord x y) = c :- pathRight (n - 1) (Coord (x + 1) y)

pathDown :: Int -> Coord -> Corridor
pathDown n _ | n <= 0 = mempty
pathDown n c@(Coord x y) = c :- pathDown (n - 1) (Coord x (y + 1))
