module PingPong.Player.SocketPlayer (player) where

import Network.Simple.TCP
import Network.Socket.ByteString as ByteString
import Data.ByteString.UTF8 as BSUTF8
import PingPong.Model
import PingPong.Player
import System.Process
import Data.Geometry
import Data.Colour
import Data.Colour.Names
import Control.Concurrent

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
socketTerminate = connect "127.0.0.1" "1240" $ \(connectSocket, connectRemoteAddr) -> do
  ByteString.send connectSocket (BSUTF8.fromString "terminate")
  return ()
  
socketAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
socketAction t h b a = connect "127.0.0.1" "1240" $ \(connectSocket, connectRemoteAddr) -> do
  let message = writeState t h b a ++ "%"
  ByteString.send connectSocket (BSUTF8.fromString message)
  answer <- ByteString.recv connectSocket 4096
  return $ readMotion (BSUTF8.toString answer)
