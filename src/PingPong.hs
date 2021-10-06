module PingPong where

import           PingPong.Model

-- import           PingPong.Simulation.Recording
import           PingPong.Simulation.Realtime

import           PingPong.Player
import qualified PingPong.Player.SoloboloPlayer as SoloboloPlayer
import qualified PingPong.Player.NativePlayer as NativePlayer
import qualified PingPong.Player.ArgPlayer    as ArgPlayer
import qualified PingPong.Player.SocketPlayer as SocketPlayer

main :: IO ()
--main = play SocketPlayer.player NativePlayer.player
main = playWithCollision SoloboloPlayer.collision 
       SoloboloPlayer.player NativePlayer.player

       