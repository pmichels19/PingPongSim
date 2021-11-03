module PingPong where

import           PingPong.Model

--import           PingPong.Simulation.Recording
import           PingPong.Simulation.Realtime

import           PingPong.Player
import qualified PingPong.Player.NativePlayer as NativePlayer
import qualified PingPong.Player.ArgPlayer    as ArgPlayer
import qualified PingPong.Player.SocketPlayer as SocketPlayer
import qualified PingPong.Player.SoloboloPlayer as SoloboloPlayer
import qualified PingPong.Player.DummyPlayer as DummyPlayer
import qualified PingPong.Player.TestPlayer as TestPlayer

main :: IO ()
main = play NativePlayer.player SoloboloPlayer.player
--main = playWithCollision NativePlayer.collision 
--       SocketPlayer.player NativePlayer.player

       