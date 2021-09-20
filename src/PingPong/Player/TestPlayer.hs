module PingPong.Player.TestPlayer (player) where

import Control.Lens
import Data.Geometry

import PingPong.Model
import PingPong.Player

import Data.Colour
import Data.Colour.Names

player :: Player
player = defaultPlayer
  { name    = "Pieter's Sick Player"
  , arm     = wavyArm
  , foot    = wavyFoot
  , action  = wavyAction
  }

gradient :: Float -> Colour Float
gradient x = blend x darkviolet crimson

wavyArm :: Arm
wavyArm = [ Link  (gradient 0.1) 0.5
          , Joint (gradient 0.2) (pi / 12)
          , Link  (gradient 0.3) 0.4
          , Joint (gradient 0.4) (pi / 6)
          , Link  (gradient 0.5) 0.3
          , Joint (gradient 0.6) (pi / 4)
          , Link  (gradient 0.7) 0.2
          , Joint (gradient 0.8) (pi / 2)
          , Link  (gradient 0.9) 0.1
          ]

wavyFoot :: Float
wavyFoot = 1.5

wavyAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
wavyAction t hit ballState arm = do
    let test = filter isJoint arm
    print(test)
    let joint1Rotation = sin (2.0 * t)
    let joint2Rotation = cos (2.1 * t)
    let joint3Rotation = sin (2.2 * t)
    let joint4Rotation = - (joint1Rotation + joint2Rotation + joint3Rotation)
    return [  joint1Rotation
            , joint2Rotation
            , joint3Rotation
            , joint4Rotation
           ]

