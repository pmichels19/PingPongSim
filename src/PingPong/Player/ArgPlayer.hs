module PingPong.Player.ArgPlayer (player, collision) where

import PingPong.Model
import PingPong.Player
import PingPong.Simulation
import PingPong.Simulation.Collision

import System.Process

import Data.Colour
import Data.Colour.SRGB
import Data.Geometry
import Data.Ext

import Control.Lens

player :: Player
player = defaultPlayer 
       { name   = "Argh Player"
       , arm    = argArm 
       , foot   = argFoot
       , action = argAction 
       }

linkColor :: Colour Float
linkColor = sRGB 0.8 0.4 0.0

jointColor :: Colour Float
jointColor = sRGB 0.8 0.8 0.9

batColor :: Colour Float
batColor = sRGB 0.2 0.3 0.4

argArm :: Arm
argArm = [ Link  linkColor  0.2
         , Joint jointColor 0  
         , Link  linkColor  0.3
         , Joint jointColor 0 
         , Link  linkColor  0.5
         , Joint jointColor 0 
         , Link  batColor   0.1 
         ]

argFoot :: Float
argFoot = 1.7



argAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
argAction t h b a = do
  let message = "action\n" ++ writeState t h b a ++ "%"
  (errCode, outRPL, errRPL) <- readProcessWithExitCode "python3" ["src/PingPong/Player/ArgPlayer.py", message] ""
  if not $ null errRPL then error errRPL 
                       else return $ readMotion outRPL


-- FOR EXERCISE B1 --

collision :: CollisionChecker
collision state1 state2 = do
  let message = "collision\n" ++ writeCollisionInput state1 state2 ++ "%"
  (errCode, outRPL, errRPL) <- readProcessWithExitCode "python3" ["src/PingPong/Player/ArgPlayer.py", message] ""
  if not $ null errRPL then error errRPL 
                       else return $ readCollisionOutput outRPL

-- FOR EXERCISE B2 --

plan :: (Float, Arm) -- location and description of the arm
     -> (Point 2 Float, Vector 2 Float, Vector 2 Float) -- desired point of collision, orientation of bat, and velocity of the bat
     -> IO (Maybe ([Float], [Float])) -- output position and angular velocity of the arm
plan xarm goal = do
  let message = "plan\n" ++ writePlanInput xarm goal ++ "%"
  (errCode, outRPL, errRPL) <- readProcessWithExitCode "python3" ["src/PingPong/Player/ArgPlayer.py", message] ""
  if not $ null errRPL then error errRPL 
                       else return $ readPlanOutput outRPL
