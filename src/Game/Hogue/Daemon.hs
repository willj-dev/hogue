{-|
Module      : Game.Hogue.Daemon
Description : Ongoing effects and mechanics that are triggered each turn, such as monster movement.
-}

module Game.Hogue.Daemon (Daemon, DaemonException) where

import Game.Hogue.Level ( Level )

-- This should be something like State Level DaemonResult?
data Daemon = Daemon (Level -> Either DaemonException Level)

data DaemonException = DaemonException -- todo
