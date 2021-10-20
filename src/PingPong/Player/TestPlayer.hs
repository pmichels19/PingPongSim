module PingPong.Player.TestPlayer (player, collision, plan) where

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
  { name    = "Test Player"
  , arm     = testArm
  , foot    = testFoot
  , action  = testAction
  }

gradient :: Float -> Colour Float
gradient x = blend x forestgreen lawngreen

testArm :: Arm
testArm = [ Link  black 0.1
           , Joint black ((-1)/4 * pi)
           , Link  black 0.05
           , Joint black (pi / 2)
           , Link  black 0.05
           , Joint black (pi / 2)
           , Link  black 0.05
           , Joint black (pi / 2)
           , Link  black 0.1
        ]

testFoot :: Float
testFoot = 1.5

testAction :: Float -> (Float, Item) -> BallState -> Arm -> IO Motion
testAction t _ _ _ = return [ 0, 0, 0, 0 ]

-- FOR EXERCISE B1 --

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


-- FOR EXERCISE B2 --

-- For a given robot arm and a desired configuration and velocity of the bat,
-- Check if it is possible to reach this desired configuration. If so, return
-- the list of configuration parameters (joint angles) and velocities (angular
-- speeds).
plan :: (Float, Arm) -- location and description of the arm
     -> (Point 2 Float, Vector 2 Float, Vector 2 Float) -- desired point of collision, orientation of bat, and velocity of the bat
     -> IO (Maybe ([Float], [Float])) -- output position and angular velocity of the arm

plan (x, arm) (pos, ori, vel) = do
  return Nothing