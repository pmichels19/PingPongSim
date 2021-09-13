module PingPong.Player.NativePlayer (player) where

import Control.Lens
import Data.Geometry

import PingPong.Model
import PingPong.Player

import Data.Colour
import Data.Colour.Names

player :: Player
player = defaultPlayer
  { name    = "Native Player"
  , arm     = wavyArm
  , foot    = wavyFoot
  , action  = wavyAction
  }

gradient :: Float -> Colour Float
gradient x = blend x forestgreen lawngreen

wavyArm :: Arm
wavyArm = [ Link  (gradient 0.1) 0.2
          , Joint (gradient 0.2) (-0.3)
          , Link  (gradient 0.3) 0.2
          , Joint (gradient 0.4) (0.2)
          , Link  (gradient 0.5) 0.2
          , Joint (gradient 0.6) (0.2)
          , Link  (gradient 0.7) 0.2
          , Joint (gradient 0.8) (-0.1)
          , Link  (gradient 0.9) 0.1
          ]

wavyFoot :: Float
wavyFoot = 1.5

wavyAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
wavyAction t _ _ _ = return [ -2 * sin (2.2 * t)
                            , -2 * cos (2.3 * t)
                            ,  2 * sin (2.4 * t)
                            ,  2 * cos (2.5 * t)
                            ]

