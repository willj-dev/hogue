{-|
Module      : Game.Hogue.Item
Description : Things to pick up and put in a backpack!
-}

module Game.Hogue.Item (Item(..)) where

data Item
  = GoldPile Int -- ^ some pieces of gold
