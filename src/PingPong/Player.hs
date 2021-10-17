module PingPong.Player where

import PingPong.Model
import Data.Geometry
import Control.Lens
import Data.Char
import Data.Colour
import Data.Ext




defaultPlayer :: Player
defaultPlayer = Player
  { name    = "DefaultPlayer"
  , arm     = [ Link black 1, Joint black 0, Link black 0.1 ]
  , initArm = [ Link black 1, Joint black 0, Link black 0.1 ]
  , foot    = 1.5
  , prepare = return ()
  , terminate = return ()
  , action  = const $ const $ const $ const $ return [0]
  , stretch = defaultStretch
  , dance   = defaultDance
  } 

noPlayer :: Player
noPlayer = defaultPlayer
  { name = "nobody"
  , arm = [Joint black 0]
  }


-- | Default stretching behaviour.
defaultStretch :: Float -> Arm -> IO Motion
defaultStretch t _ = return $ propcap $ map (* cos t) [-3, 6, -6, 6, -6]

-- | Default dancing behaviour.
defaultDance :: Float -> Arm -> IO Motion
defaultDance t _ = return [ 5 * sin (2.5 * (t + 0.0))
                          , 5 * sin (2.5 * (t + 0.3))
                          , 5 * sin (2.5 * (t + 0.6))
                          , 5 * sin (2.5 * (t + 0.9))
                          , 5 * sin (2.5 * (t + 1.2))
                          ]

propcap :: [Float] -> [Float]
propcap xs | m < 2 = xs
           | otherwise = map (\x -> 2 * x / m) xs
  where m = maximum $ map abs xs         





-- converting game state to and from String

writeState :: Float -> (Float, Item) -> BallState -> Arm -> String
writeState time hit bs a = printTime time ++ printHit hit ++ printBallState bs ++ printArm a

printTime :: Float -> String
printTime t = show t ++ "\n"

printHit :: (Float, Item) -> String
printHit (t, i) = show t ++ " " ++ map toLower (show i) ++ "\n"

printBallState :: BallState -> String
printBallState bs = "loc " ++ printPoint (loc bs) ++ "\n"
                 ++ "dir " ++ printPoint (origin .+^ dir bs) ++ "\n"

printPoint :: Show r => Point 2 r -> String
printPoint p = (show $ view xCoord p) ++ " " ++ (show $ view yCoord p)

printSegment :: Show r => LineSegment 2 () r -> String
printSegment s = printPoint (s ^. start ^. core) ++ " " ++ printPoint (s ^. end ^. core)
  
printArm :: Arm -> String
printArm arm = unwords (map printElement arm)

printElement :: Element -> String
printElement (Joint _ x) = "joint " ++ show x
printElement (Link  _ x) = "link "  ++ show x

printMotion :: Motion -> String
printMotion = unwords . map show

readMotion :: String -> Motion
readMotion = map read . words

-- communication for Collisions

-- Convert input to the collision checker to a string.
writeCollisionInput :: (Float, Point 2 Float, LineSegment 2 () Float) 
                    -> (Float, Point 2 Float, LineSegment 2 () Float) 
                    -> String

writeCollisionInput (time1, point1, segment1) (time2, point2, segment2) 
  =  "time " ++ show time1 ++ " point " ++ printPoint point1 ++ " segment " ++ printSegment segment1 ++ "\n"
  ++ "time " ++ show time2 ++ " point " ++ printPoint point2 ++ " segment " ++ printSegment segment2 ++ "\n"


writeCollisionOutput :: Maybe (Float, Point 2 Float, Vector 2 Float) -> String
writeCollisionOutput Nothing = "no collision"
writeCollisionOutput (Just (t, Point2 xp yp, Vector2 xv yv)) = "time " ++ show t ++ " point " ++ show xp ++ " " ++ show yp ++ " vector " ++ show xv ++ " " ++ show yv

-- Read the output for the collision checker from a string.
readCollisionOutput :: String -> Maybe (Float, Point 2 Float, Vector 2 Float)
readCollisionOutput "no collision" = Nothing
readCollisionOutput str = readCollisionWords $ words str

readCollisionWords :: [String] -> Maybe (Float, Point 2 Float, Vector 2 Float)
readCollisionWords ["no", "collision"] = Nothing
readCollisionWords [_, t, _, xp, yp, _, xv, yv] = Just (read t, Point2 (read xp) (read yp), Vector2 (read xv) (read yv))
readCollisionWords ws = error $ "cannot parse message: " ++ unwords ws

-- communication for Plan

-- Convert input to the collision checker to a string.
writePlanInput :: (Float, Arm) -- location and description of the arm
               -> (Point 2 Float, Vector 2 Float, Vector 2 Float)  -- desired point of collision, orientation of bat, and velocity of the bat
               -> String
writePlanInput (x, arm) (p, n, v) 
  =  "foot " ++ show x ++ " arm " ++ printArm arm ++ "\n"
  ++ "point " ++ printPoint p ++ " normal " ++ printPoint (origin .+^ n) ++ " velocity " ++ printPoint (origin .+^ v) ++ "\n"
  

writePlanOutput :: Maybe ([Float], [Float]) -> String
writePlanOutput Nothing = "impossible"
writePlanOutput (Just (angles, speeds)) = "angles " ++ printMotion angles ++ "\n"
                                       ++ "speeds " ++ printMotion speeds ++ "\n"

-- Read the output for the collision checker from a string.
readPlanOutput :: String -> Maybe ([Float], [Float]) -- output position and angular velocity of the arm
readPlanOutput "impossible" = Nothing
readPlanOutput str = case lines str of [l1, l2] -> Just (map read $ tail $ words l1, map read $ tail $ words l2)
                                       _        -> error $ "readPlanOutput: cannot parse message \"" ++ str ++ "\""