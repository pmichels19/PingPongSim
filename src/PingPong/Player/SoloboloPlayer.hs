module PingPong.Player.SoloboloPlayer (player, collision) where

import Control.Lens
import Data.Geometry
import Data.Ext

import PingPong.Model
import PingPong.Player

import PingPong.Simulation.Collision

import Data.Colour
import Data.Colour.Names

player :: Player
player = defaultPlayer
  { name    = "Solobolo"
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

-- Check for collision between a moving point p and a moving line segment qr, 
-- given their locations at two different times.
-- If a collision occurs, return the time of collision, the location of p
-- at collision time, and the resulting velocity vector of p at that time.
collision :: CollisionChecker
collision (time1, point1, segment1) (time2, point2, segment2) = do
  let Point2 xp1 yp1 = point1
      Point2 xp2 yp2 = point2
      Point2 xq1 yq1 = segment1 ^. start ^. core  
      Point2 xq2 yq2 = segment2 ^. start ^. core
      Point2 xr1 yr1 = segment1 ^. end   ^. core
      Point2 xr2 yr2 = segment2 ^. end   ^. core
  return Nothing
