module PingPong.Simulation where

import PingPong.Model
import PingPong.Draw

import Data.Geometry hiding (init, head, zero, replicate)
import Data.Geometry.Matrix
import Data.Geometry.Transformation

import Data.Fixed
import Data.Ext
import Data.List hiding (intersect)
import Data.Foldable
import Control.Lens hiding (snoc)

import Convert
import Transformation

import Data.Vinyl.CoRec

import Debug.Trace

import System.Random

serveBall :: Bool -> IO BallState
serveBall p = do
  h <- randomRIO (0.9, 1)
  x <- randomRIO (1.3, 1.4)
  y <- randomRIO (-0.1, 0)
  return $ BallState (Point2 0 h) (Vector2 (if p then x else -x) y)

-- | Maximum speed in radians per second of joint rotation.
maxSpeed :: Float
maxSpeed = 2

-- Duration of non-rally game phases in seconds
beforeGameTime  = 4
beforeRallyTime = 4
afterRallyTime  = 2
afterGameTime   = 10

-- updating

getPlayer :: Bool -> State -> Player
getPlayer True  = p1
getPlayer False = p2

act :: Bool -> State -> IO Motion
act b st = case phase st of BeforeGame _  -> stretch (getPlayer b st) (time st) $ arm $ getPlayer b st
                            BeforeRally _ -> straightenUp b st
                            DuringRally   -> actDuringRally b st
                            AfterGame _   -> dance (getPlayer b st) (time st) $ arm $ getPlayer b st
                            _             -> return $ replicate 5 0

straightenUp :: Bool -> State -> IO Motion
straightenUp True  st = return $ straighten (p1 st)
straightenUp False st = return $ straighten (p2 st)

straighten :: Player -> Motion
straighten p = 
  let vals = map modAngle $ map (\(Joint _ v) -> v) $ filter isJoint $ arm p 
      gals = map modAngle $ map (\(Joint _ v) -> v) $ filter isJoint $ initArm p 
  in zipWith (\v g -> modAngle $ g - v) vals gals

modAngle :: Float -> Float
modAngle x = (x + pi) `mod'` (2 * pi) - pi

actDuringRally :: Bool -> State -> IO Motion
actDuringRally True  st = action (p1 st) (time st) (head $ hits st) (ball st) (arm $ p1 st)
actDuringRally False st = act True (flipState st)
-- fmap flipMotion $
-- don't flip resulting motion -> motion is always in local perspective

update :: Float -> State -> IO State
update deltaTime st = do
  om1 <- act True  st
  om2 <- act False st
  let omb = dir $ ball st
  let initialTime  = time st
      goalTime     = initialTime + deltaTime
      initialState = st {m1 = om1, m2 = om2} -- , mb = omb}
      finalState   = case phase st of
                       BeforeRally _ -> updateUntilGhost goalTime initialState {frame = frame st + 1}
                       _ -> updateUntil goalTime initialState {frame = frame st + 1}
  curvedState <- curveBall deltaTime finalState
  perturbedState <- perturb deltaTime curvedState
  updatePhase deltaTime perturbedState

-- update the phase: count down timer, and if reached 0, take approriate action
-- if phase is DuringRally, then check the last thing that was hit.
updatePhase :: Float -> State -> IO State
updatePhase delta st = f $ phase st
  where 
    f (BeforeGame t)  | t > delta = return $ st {phase = BeforeGame  $ t - delta}
                      | otherwise = initBeforeRally st 
    f (BeforeRally t) | t > delta = return $ st {phase = BeforeRally $ t - delta}
                      | otherwise = initDuringRally st
    f (AfterRally t)  | t > delta = return $ st {phase = AfterRally  $ t - delta}
                      | otherwise = initBeforeRally st
    f (AfterGame t)   | t > delta = return $ st {phase = AfterGame   $ t - delta}
                      | otherwise = return $ st {phase = GameOver} -- the game is over, simulation should stop
    f DuringRally     | testScore (map snd $ hits st) (loc $ ball st) == Nothing = return $ st
                      | otherwise = updateScore (unJust $ testScore (map snd $ hits st) (loc $ ball st)) (score st) st

initBeforeGame :: State -> IO State
initBeforeGame st = return $ st { phase = BeforeGame beforeGameTime
                                , score = (0, 0)
                                , ball  = BallState (Point2 (-1) 0.6) (Vector2 0.4 1)
                                , p1    = (p1 st) {initArm = arm (p1 st)}
                                , p2    = (p2 st) {initArm = arm (p2 st)}
                                }

initBeforeRally :: State -> IO State
initBeforeRally st = do
  b <- serveBall True
  return $ st { phase = BeforeRally beforeRallyTime
              , ball  = b {dir = Vector2 0 0}
              }

initDuringRally :: State -> IO State
initDuringRally st = do
  let (i, j) = score st
      p      = (i + j) `mod` 4 < 2
  b <- serveBall p
  return $ st { phase = DuringRally
              , hits  = [(0, Bat $ if p then Opponent else Self)] 
              , ball  = (ball st) {dir = dir b}
              }

initAfterRally :: State -> IO State
initAfterRally st = return $ st {phase = AfterRally afterRallyTime}

initAfterGame :: State -> IO State
initAfterGame st = return $ st {phase = AfterGame  afterGameTime}

unJust (Just x) = x

testScore :: [Item] -> Point 2 Float -> Maybe Bool
testScore [] _ = Nothing
testScore [_] _ = Nothing
testScore (Table Self : Bat Opponent : _) _ = Nothing
testScore (Table Opponent : Bat Self : _) _ = Nothing
testScore (Bat Self : Table Self : _) _ = Nothing
testScore (Bat Opponent : Table Opponent : _) _ = Nothing
testScore (_ : Bat Opponent : _) (Point2 x y) | y > 0.5 && x > 0 && x < 1 = Just False
                                              | otherwise                 = Just True
testScore (_ : Bat Self : _) (Point2 x y) | y > 0.5 && x > -1 && x < 0 = Just True
                                          | otherwise                  = Just False
testScore (_ : Table Opponent : _) _ = Just True
testScore (_ : Table Self : _) _ = Just False
testScore (Other a : Other b : is) p = testScore (Other b : is) p
testScore _ _ = Nothing

updateScore :: Bool -> (Int, Int) -> State -> IO State
updateScore True  (a, b) st | a >= 10 && b <  a  = initAfterGame $ st {score = (a + 1, b)}
updateScore False (a, b) st | a <  b  && b >= 10 = initAfterGame $ st {score = (a, b + 1)}
updateScore True  (a, b) st = initAfterRally $ st {score = (a + 1, b)}
updateScore False (a, b) st = initAfterRally $ st {score = (a, b + 1)}

perturb :: Float -> State -> IO State
perturb deltaTime st = do
  dx <- randomRIO (-amount, amount)
  dy <- randomRIO (-amount, amount)
  return $ st {ball = (ball st) {dir = dir (ball st) ^+^ Vector2 dx dy}}
    where amount = 0.025 * deltaTime


curveBall :: Float -> State -> IO State
curveBall deltaTime st = return $ st {ball = (ball st) {dir = decay *^ dir (ball st) ^+^ 2 * deltaTime *^ down}}
  where decay = (1 - 0.05) ** deltaTime

-- | Update state using fixed motion until the goal time.
--   Will recurse until the next collision event is later than the goal time.
updateUntil :: Float -> State -> State
updateUntil deadline st0 | deadline == time st0 = st0
                         | otherwise =
  let st1 = updateUntilRaw deadline st0
      t0  = time st0
      t1  = time st1
      b0  = loc $ ball st0
      b1  = loc $ ball st1
      collide (i, s0, s1) = collide' i (t0, b0, s0) (t1, b1, s1)
      repeated (t, i, _) = i /= Air && (fst $ head $ hits st0) >= t0 && i == (snd $ head $ hits st0)
      candidates = sort $ filter (not . repeated) $ map collide $ zip3 items (segmentsAt st0) (segmentsAt st1)
      (t, i, v) = head $ candidates ++ [(t1, Air, dir $ ball st1)]
  in -- traceShow (t, v) $  
     updateUntil deadline $ updateUntilRaw t st0 { ball = (ball st0) {dir = v}
                                                 , hits = newHits (hits st0) (t, i)
                                                 }

items :: [Item]
items = map item $ [0..]

item :: Int -> Item
item 0 = Bat Self
item 1 = Bat Opponent
item 2 = Table Self
item 3 = Table Opponent
item 10 = Net
item x = Other x

newHits :: [(Float, Item)] -> (Float, Item) -> [(Float, Item)]
newHits os (_, Air) = os
newHits os n        = n : os

-- | Updates state without checking for collisions.
updateUntilRaw :: Float -> State -> State
updateUntilRaw deadline st | deadline == time st = st
                           | otherwise =
  let f   = deadline - time st
      op1 = p1 st
      op2 = p2 st
      ob  = ball st
      np1 = op1 {arm = performMotion f (m1 st) op1}
      np2 = op2 {arm = performMotion f (m2 st) op2}
      nb  = straightBallStep f ob
  in st { time = deadline
        , p1   = np1
        , p2   = np2
        , ball = nb
        }

-- | Updates state without checking for collisions.
updateUntilGhost :: Float -> State -> State
updateUntilGhost deadline st | deadline == time st = st
                           | otherwise =
  let f   = deadline - time st
      op1 = p1 st
      op2 = p2 st
      ob  = ball st
      np1 = op1 {arm = performMotionRaw f (m1 st) (arm op1)}
      np2 = op2 {arm = performMotionRaw f (m2 st) (arm op2)}
      nb  = ob
  in st { time = deadline
        , p1   = np1
        , p2   = np2
        , ball = nb
        }


segmentsAt :: State -> [LineSegment 2 () Float]
segmentsAt st =  (last . toList . edgeSegments) (playerGeom True  $ p1 st)
              :  (last . toList . edgeSegments) (playerGeom False $ p2 st)
              :  listEdges table 
              ++ listEdges room
              ++ [net] 
              ++ (init . toList . edgeSegments) (playerGeom True  $ p1 st)
              ++ (init . toList . edgeSegments) (playerGeom False $ p2 st)
 

playerGeom :: Bool -> Player -> PolyLine 2 () Float
playerGeom b p = playerTransform (foot p) b $ evaluateP (arm p) 


playerTransform :: (IsTransformable g, NumType g ~ Float, Dimension g ~ 2) => Float -> Bool -> g -> g
playerTransform d True = translateBy $ Vector2 d 0
playerTransform d False = scaleBy (Vector2 (-1) 1) . translateBy (Vector2 d 0)

performMotion :: Float -> Motion -> Player -> Arm 
performMotion f m p | all (== 0) m = arm p
                    | otherwise =
  let na = performMotionRaw f (map cap m) $ arm p
  in if or [ intersectsExact s t
           | s <- filter (not . degenerate) $ armSegments p {arm = na} True
           , t <- listEdges table ++ listEdges room
           ]
     then performMotion f (strip0 m) p
     else na

strip0 :: [Float] -> [Float]
strip0 [] = []
strip0 (x : xs) | x /= 0 = 0 : xs
                | x == 0 = x : strip0 xs

cap :: Float -> Float
cap x | x < -maxSpeed = -maxSpeed
      | x >  maxSpeed =  maxSpeed
      | otherwise     =  x


intersectsExact :: LineSegment 2 () Float -> LineSegment 2 () Float -> Bool
--intersectsExact s t | converf s `intersects` converf t = traceShow (s, t) $ True
--                    | otherwise                        = False
intersectsExact s t = converf s `intersects` converf t

converf :: LineSegment 2 () Float -> LineSegment 2 () Rational
converf = endPoints . core . traverse %~ realToFrac


degenerate :: Eq r => LineSegment 2 () r -> Bool
degenerate s = s ^. start . core == s ^. end . core


{-  
  let rs, rt :: LineSegment 2 () Rational
      rs = s & endPoints %~ (traverse %~ realToFrac)
      rt = t & endPoints %~ (traverse %~ realToFrac)
  in intersects rs rt
-}

-- needs to check:
-- * for too fast motion
-- * collision with table
-- * self-collision
-- * reaching over origin?

performMotionRaw :: Float -> Motion -> Arm -> Arm 
performMotionRaw f m (l@(Link _ _) : arm) = l : performMotionRaw f m arm
performMotionRaw f (x : xs) (Joint c a : arm) = Joint c (applyMotion f x a) : performMotionRaw f xs arm
performMotionRaw f _ arm = arm

applyMotion :: Float -> Float -> Float -> Float
applyMotion f x a = a + f * x



-- ball step
straightBallStep :: Float -> BallState -> BallState
straightBallStep f st = st { loc = loc st .+^ f *^ dir st }

simpleBallStep :: Float -> BallState -> BallState
simpleBallStep f st = st { loc = loc st .+^ f *^ dir st
                         , dir = decay *^ dir st ^+^ 2 * f *^ down
                         }
  where decay = (1 - 0.05) ** f


-- | Reflect vector 'a' in a line with direction vector 'b'.
reflectVector :: Vector 2 Float -> Vector 2 Float -> Vector 2 Float
reflectVector a b = reflection (angle (Vector2 1 0) b) `transformBy` a

-- | Find the angle between two vectors, in counter-clockwise order, from the first to the second.
angle :: Vector 2 Float -> Vector 2 Float -> Float
angle (Vector2 x1 y1) (Vector2 x2 y2) = atan2 (x1 * y2 - y1 * x2) (x1 * x2 + y1 * y2)

down :: Vector 2 Float
down = Vector2 0 (-1)





collide' :: Item
         -> (Float, Point 2 Float, LineSegment 2 () Float) 
         -> (Float, Point 2 Float, LineSegment 2 () Float) 
         -> (Float, Item, Vector 2 Float)

collide' i (t0, Point2 x0 y0, s0) (t1, Point2 x1 y1, s1) 
  | y1 >  0 = (t1, Air    , Vector2 (x1 - x0) (y1 - y0) ^/ (t1 - t0))
  | y1 <= 0 = (t1, Other 0, Vector2 (x1 - x0) (y0 - y1) ^/ (t1 - t0))
