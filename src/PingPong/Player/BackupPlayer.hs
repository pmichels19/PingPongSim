module PingPong.Player.BackupPlayer (player, collision) where

import Control.Lens
import Data.Geometry
import Data.Ext

import PingPong.Model
import PingPong.Player

import PingPong.Simulation.Collision

import Data.Colour
import Data.Colour.Names
import Data.Maybe

player :: Player
player = defaultPlayer
  { name    = "Solobolo backup"
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

-- covered
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
collision (time1, point1, segment1) (time2, point2, segment2) = 
  let Point2 a b = point1
      Point2 xp2 yp2 = point2
      Point2 e f = segment1 ^. start ^. core  
      Point2 xq2 yq2 = segment2 ^. start ^. core
      Point2 c d = segment1 ^. end   ^. core
      Point2 xr2 yr2 = segment2 ^. end   ^. core
      g = xp2 - a
      h = yp2 - b
      i = xr2 - c
      j = yr2 - d
      k = xq2 - e
      l = yq2 - f
      abcA = getabcA g h i j k l
      abcB = getabcB a b c d e f g h i j k l
      abcC = getabcC a b c d e f
      abcD = getabcD abcA abcB abcC
      toi = getTOI abcA abcB abcC abcD
      point = Point2 a b
      velocity = Vector2 a b
  in return Nothing
  -- in return (toi, point, velocity)
  -- return (toi, getCollisionPoint toi (c + toi * i) (d + toi * j) (e + toi * k) (f + toi * l) (a + toi * g) (b + toi * h), Vector2 1 1)
  -- if isNothing toi
  --   then return Nothing
  --   else return $ Just (1, Point2 1 1, Vector2 1 1)
    -- else return Nothing
    -- else return getCollision toi (c + toi * i) (d + toi * j) (e + toi * k) (f + toi * l) (a + toi * g) (b + toi * h)

getCollisionPoint :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Maybe (Point 2 Float)
getCollisionPoint toi rxtoi rytoi qxtoi qytoi pxtoi pytoi 
    | sqrt ( (rxtoi - qxtoi)^2 + (rytoi - qytoi)^2 ) - (sqrt ( (rxtoi - pxtoi)^2 + (rytoi - pytoi)^2 ) + sqrt ( (pxtoi - qxtoi)^2 + (pytoi - qytoi)^2 )) < 0.0000001 = Just (Point2 pxtoi pytoi)
    | otherwise = Nothing

-- covered
-- gets the time of impact based on the a b c and d for the quadratic formula
getTOI :: Float -> Float -> Float -> Float -> Float
getTOI a b c d | d < 0 = -1
               | d == 0 = (-b) / (2 * a)
               | otherwise = getEarliest ((-b + sqrt d) / (2 * a)) ((-b + sqrt d) / (2 * a))

-- function to get lowest TOI from the two possible TOI's
getEarliest :: Float -> Float -> Float
getEarliest t1 t2 | (t1 < 0 || t1 > 1) && (t2 < 0 || t2 > 1) = -1
             | t1 < 0 || t1 > 1 = t2
             | t1 < t2 = t1
             | otherwise = t2

-- abc formula - covered
-- a = xp1      g = xp2 - xp1
-- b = yp1      h = yp2 - yp1
-- c = xr1      i = xr2 - xr1
-- d = yr1      j = yr2 - yr1
-- e = xq1      k = xq2 - xq1
-- f = yq1      l = yq2 - yq1
-- get the discriminant
getabcD :: Float -> Float -> Float -> Float
getabcD a b c = (b * b) - (4 * a * c)

-- get the A of the abc formula
getabcA :: Float -> Float -> Float -> Float -> Float -> Float -> Float
getabcA g h i j k l = -((i * l) - (h * k) + (g * l) + (i * h) + ((i + k) * j) - ((g + i) * j))

-- get the b of the abc formula
getabcB :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
getabcB a b c d e f g h i j k l = -((d * g) - (c * l) - (k * b) - (h * e) + (l * a) + (g * f) + (i * b) + (c * h) + ((i + k) * d) + ((c + e) * j) - ((f + d) * i) - ((a + c) * j))

-- get the c of the abc formula
getabcC :: Float -> Float -> Float -> Float -> Float -> Float -> Float
getabcC a b c d e f = ((c + e) * d) - ((a + c) * d) + (a * f) - (c * f) - (b * e) + (c * b)