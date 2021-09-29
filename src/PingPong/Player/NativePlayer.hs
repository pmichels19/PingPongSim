module PingPong.Player.NativePlayer (player, collision) where

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
