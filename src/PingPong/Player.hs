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
  
printArm :: Arm -> String
printArm arm = unwords (map printElement arm)

printElement :: Element -> String
printElement (Joint _ x) = "joint " ++ show x
printElement (Link  _ x) = "link "  ++ show x

readMotion :: String -> Motion
readMotion = map read . words

-- communication for Collisions

-- Convert input to the collision checker to a string.
writeCollisionInput :: (Float, Point 2 Float, LineSegment 2 () Float) 
                    -> (Float, Point 2 Float, LineSegment 2 () Float) 
                    -> String

writeCollisionInput (time1, point1, segment1) (time2, point2, segment2) =
  let Point2 xp1 yp1 = point1
      Point2 xp2 yp2 = point2
      Point2 xq1 yq1 = segment1 ^. start ^. core
      Point2 xq2 yq2 = segment2 ^. start ^. core
      Point2 xr1 yr1 = segment1 ^. end   ^. core
      Point2 xr2 yr2 = segment2 ^. end   ^. core
  in      "time " ++ show time1 ++ " point " ++ show xp1 ++ " " ++ show yp1
      ++ " segment " ++ show xq1 ++ " " ++ show yq1 ++ " " ++ show xr1 ++ " " ++ show yr1 ++ "\n"
      ++  "time " ++ show time2 ++ " point " ++ show xp2 ++ " " ++ show yp2
      ++ " segment " ++ show xq2 ++ " " ++ show yq2 ++ " " ++ show xr2 ++ " " ++ show yr2 ++ "\n"


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