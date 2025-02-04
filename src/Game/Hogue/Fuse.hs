{-|
Module      : Game.Hogue.Fuse
Description : Ongoing effects and mechanics that run for a set number of turns before expiring (possibly with a bang)
-}

module Game.Hogue.Fuse (Fuse(..), FuseException) where

import Game.Hogue.Level ( Level )

-- Possibly some kind of state monad thing, where one of the results contains the same fuse with decremented timer?
data Fuse = Fuse
  { timer   :: Int -- how many more turns will this fuse run for?
  , ongoing :: Level -> Either FuseException Level -- ongoing effect while the fuse is active
  , bang    :: Level -> Either FuseException Level -- what happens when the fuse runs out?
  }

data FuseException = FuseException -- todo
