module PingPong.Player.ArgPlayer (player) where

import System.Process

import PingPong.Model
import PingPong.Player
import PingPong.Simulation

import Data.Colour
import Data.Colour.SRGB

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
  let message = writeState t h b a ++ "%"
  (errCode, outRPL, errRPL) <- readProcessWithExitCode "python3" ["src/PingPong/Player/ArgPlayer.py", message] ""
  if not $ null errRPL then error errRPL 
                       else return $ readMotion outRPL

