module PingPong where

import           PingPong.Model

--import           PingPong.Simulation.Recording
import           PingPong.Simulation.Realtime

import           PingPong.Player
import qualified PingPong.Player.NativePlayer as NativePlayer
import qualified PingPong.Player.ArgPlayer    as ArgPlayer
import qualified PingPong.Player.SocketPlayer as SocketPlayer
import qualified PingPong.Player.SoloboloPlayer as SoloboloPlayer

main :: IO ()
main = play SoloboloPlayer.player NativePlayer.player
--main = playWithCollision NativePlayer.collision 
--       SocketPlayer.player NativePlayer.player