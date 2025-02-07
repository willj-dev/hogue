{-|
Module      : Game.Hogue.Coord
Description : A data type for storing coordinates on the game's map, and tools for working with those values.
-}

module Game.Hogue.Coord (Coord, coord, (.+.), (.-.)) where

-- | A coordinate in the game's map. (0, 0) is the top left corner; the first value is the horizontal (x) axis, and the second value is the vertical (y) axis.
type Coord = (Int, Int)

-- | A protected constructor against invalid coordinates (i.e. negative values).
coord :: Int -> Int -> Coord
coord x y | x < 0 || y < 0 = error "Invalid Coordinate!"
coord x y = (x, y)

(.+.) :: Coord -> Coord -> Coord
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

(.-.) :: Coord -> Coord -> Coord
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)
