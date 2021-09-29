module PingPong.Player.SocketPlayer (player, collision) where

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
  { name      = "Socket Player"
  , arm       = socketArm
  , foot      = socketFoot
  , action    = socketAction
  , prepare   = socketPrepare
  , terminate = socketTerminate
  }

socketArm :: Arm
socketArm = [ Link  cornflowerblue 0.3
            , Joint cornflowerblue 0
            , Link  cornflowerblue 0.3
            , Joint cornflowerblue 0
            , Link  cornflowerblue 0.3
            , Joint cornflowerblue 0
            , Link  cornflowerblue 0.3
            , Joint cornflowerblue 0
            , Link  cornflowerblue 0.3
            , Joint cornflowerblue 0
            , Link  hotpink        0.1
            ]

socketFoot :: Float
socketFoot = 1.5

socketPrepare :: IO ()
socketPrepare = do
  spawnCommand ("python3 src/PingPong/Player/SocketPlayer.py")
  threadDelay 100000
  return ()

socketTerminate :: IO ()
socketTerminate = connect "127.0.0.1" (show port) $ \(connectSocket, connectRemoteAddr) -> do
  ByteString.send connectSocket (BSUTF8.fromString "terminate")
  return ()
  
socketAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
socketAction t h b a = connect "127.0.0.1" (show port) $ \(connectSocket, connectRemoteAddr) -> do
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

