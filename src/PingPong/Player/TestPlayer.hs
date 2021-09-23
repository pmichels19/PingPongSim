module PingPong.Player.TestPlayer (player) where

import Control.Lens
import Data.Geometry

import PingPong.Model
import PingPong.Player

import Data.Colour
import Data.Colour.Names

player :: Player
player = defaultPlayer
  { name    = "Pieters' Player"
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

-- Get the angle from a joint, if a non-joint element is given, 0.0 is returned
currentJointAngle :: Element -> Float
currentJointAngle (Joint _ angle) = angle
currentJointAngle _ = 0.0

wavyAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
wavyAction t hit ballState arm = do
    let joints = filter isJoint arm
    let joint1Rot = currentJointAngle (joints !! 0)
    let joint2Rot = currentJointAngle (joints !! 1)
    let joint3Rot = currentJointAngle (joints !! 2)
    let joint4Rot = currentJointAngle (joints !! 3)
    let joint4Rotation = joint4Rot - (joint1Rot + joint2Rot + joint3Rot)
    return [  sin t
            , cos t
            , sin t
            , joint4Rotation
           ]

