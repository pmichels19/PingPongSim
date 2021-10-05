module PingPong.Player.SoloboloPlayer (player, collision) where

import PingPong.Model
import PingPong.Player

import PingPong.Simulation.Collision

import Network.Simple.TCP
import Network.Socket.ByteString as ByteString
import Data.ByteString.UTF8 as BSUTF8
import Data.Geometry
import Data.Colour
import Data.Colour.Names
import Data.Ext

import System.Process
import Control.Concurrent
import Control.Lens

-- The port you will use to communicate.
-- Change this to something unique! Otherwise, if your opponent also uses sockets
-- and uses the same port, weird things will happen.
port :: Integer
port = 1245

player :: Player
player = defaultPlayer
  { name      = "Solobolo"
  , arm       = soloboloArm
  , foot      = soloboloFoot
  , action    = soloboloAction
  , prepare   = soloboloPrepare
  , terminate = soloboloTerminate
  }

gradient :: Float -> Colour Float
gradient x = blend x darkviolet crimson

soloboloArm :: Arm
soloboloArm = [ Link  (gradient 0.1) 0.5
          , Joint (gradient 0.2) (pi / 12)
          , Link  (gradient 0.3) 0.4
          , Joint (gradient 0.4) (pi / 6)
          , Link  (gradient 0.5) 0.3
          , Joint (gradient 0.6) (pi / 4)
          , Link  (gradient 0.7) 0.2
          , Joint (gradient 0.8) (pi / 2)
          , Link  (gradient 0.9) 0.1
          ]

soloboloFoot :: Float
soloboloFoot = 1.5

soloboloPrepare :: IO ()
soloboloPrepare = do
  spawnCommand ("python3 src/PingPong/Player/SoloboloPlayer.py")
  threadDelay 100000
  return ()

soloboloTerminate :: IO ()
soloboloTerminate = connect "127.0.0.1" (show port) $ \(connectSocket, connectRemoteAddr) -> do
  ByteString.send connectSocket (BSUTF8.fromString "terminate")
  return ()
  
soloboloAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
soloboloAction t h b a = connect "127.0.0.1" (show port) $ \(connectSocket, connectRemoteAddr) -> do
  let message = "action\n" ++ writeState t h b a ++ "%"
  ByteString.send connectSocket (BSUTF8.fromString message)
  answer <- ByteString.recv connectSocket 4096
  return $ readMotion (BSUTF8.toString answer)

-- FOR EXERCISE B1 --

collision :: CollisionChecker
collision state1 state2 = connect "127.0.0.1" (show port) $ \(connectSocket, connectRemoteAddr) -> do
  let message = "collision\n" ++ writeCollisionInput state1 state2 ++ "%"
  ByteString.send connectSocket (BSUTF8.fromString message)
  answer <- ByteString.recv connectSocket 4096
  return $ readCollisionOutput (BSUTF8.toString answer)