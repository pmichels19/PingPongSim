module PingPong.Simulation.Collision (CollisionChecker, defaultCollisionChecker, verify) where

import Data.Geometry hiding (init, head, zero, replicate)
import Data.List hiding (intersect)
import Data.Foldable
import Data.Ext

import Control.Lens

import Data.Geometry.Transformation
import Transformation

-- * Implementation of collision checking.

-- | A collision checker takes as input two time stamps, with for each time stamp
--   the location of a point and a segment.
--   Assuming linear motion between the two time stamps, the checker should test
--   if a collision between the point and the segment takes place.
--   If so, it should report the time of the collision, as well as location and
--   velocity of the point as a result of the collision.
type CollisionChecker = (Float, Point 2 Float, LineSegment 2 () Float) 
                     -> (Float, Point 2 Float, LineSegment 2 () Float) 
                     -> IO (Maybe (Float, Point 2 Float, Vector 2 Float))

-- The collision checker that will be used when running the simulator through the play function.
defaultCollisionChecker :: CollisionChecker
defaultCollisionChecker = floorChecker

-- Make sure a collision checker returns something with a sensible time stamp.
verify :: CollisionChecker -> CollisionChecker
verify checker st1 st2 = do
  result <- checker st1 st2
  return $ verifyResult st1 st2 result

verifyResult :: (Float, Point 2 Float, LineSegment 2 () Float) 
             -> (Float, Point 2 Float, LineSegment 2 () Float) 
             -> Maybe (Float, Point 2 Float, Vector 2 Float)
             -> Maybe (Float, Point 2 Float, Vector 2 Float)
verifyResult _ _ Nothing = Nothing
verifyResult (t1, p1, s1) (t2, p2, s2) (Just (t, p, v)) | t <= t1 = Nothing
                                                        | t >= t2 = Nothing
                                                        | otherwise = Just (t, p, v)

-- A simple collision checker which ignores the segment, and only checks for collision with the floor.
floorChecker :: CollisionChecker
floorChecker (t1, Point2 x1 y1, _) (t2, Point2 x2 y2, _)
  | y2 >= 0   = return Nothing
  | y1 == y2  = error "Ball was already under the floor!?"
  | otherwise = let tc = t1 + (t2 - t1) * y1 / (y1 - y2)
                    xc = x1 + (x2 - x1) * y1 / (y1 - y2)
                    yc = 0
                    dx = (x2 - x1) / (t2 - t1)
                    dy = (y1 - y2) / (t2 - t1)
                in return $ Just (tc, Point2 xc yc, Vector2 dx dy)
  