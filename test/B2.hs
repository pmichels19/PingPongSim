module B2 where

import PingPong
import PingPong.Model
import PingPong.Simulation
import PingPong.Player

import Data.Geometry hiding (init, head, replicate)
import Data.Colour.Names
import Data.Ext
import Control.Monad
import Control.Lens

import Transformation

import System.Random


-- TESTING COLLISION FUNCTION OF SocketPlayer

import qualified PingPong.Player.NativePlayer as NativePlayer
import qualified PingPong.Player.SocketPlayer as SocketPlayer
import qualified PingPong.Player.SoloboloPlayer as SoloboloPlayer

main :: IO ()
main = do
  -- prepare   SoloboloPlayer.player
  testPlans SoloboloPlayer.plan
  terminate SoloboloPlayer.player


-- TEST CASES

type TestInput  = ((Float, Arm), (Point 2 Float, Vector 2 Float, Vector 2 Float))
type TestOutput = (Maybe ([Float], [Float]))
type TestCase   = (TestInput, TestOutput)

type Planner =  (Float, Arm) -- location and description of the arm
             -> (Point 2 Float, Vector 2 Float, Vector 2 Float) -- desired point of collision, orientation of bat, and velocity of the bat
             -> IO (Maybe ([Float], [Float])) -- output position and angular velocity of the arm

testcases :: [TestCase]
testcases = [ ( ( (0  , [ Link  black 0.3
                        , Joint black 0
                        , Link  black 0.1
                        ])
                , (Point2 0 0, Vector2 1 0, Vector2 1 0)
                )            
              , Nothing
              )
            , ( ( (0  , [ Link  black 0.1
                        , Joint black 0
                        , Link  black 0.05
                        , Joint black 0
                        , Link  black 0.05
                        , Joint black 0
                        , Link  black 0.05
                        , Joint black 0
                        , Link  black 0.1
                        ])
                , (Point2 0 0.1, Vector2 1 1, Vector2 0 0)
                )            
              , Just ( [(-1)/4 * pi, pi / 2, pi / 2, pi / 2], [0, 0, 0, 0] )
              )
            , ( ( (1  , [ Link  black 1
                        , Joint black 0
                        , Link  black 0.8
                        , Joint black 0
                        , Link  black 0.3
                        , Joint black 0
                        , Link  black 0.1
                        ])
                , (Point2 0.65 0.7, Vector2 0 1, Vector2 0 0)
                )            
              , Just ( [1/2 * pi + asin 0.6, pi, asin (-0.6)], [0, 0, 0] )
              )            
            , ( ( (0  , [ Link  black 0.1
                        , Joint black 0
                        , Link  black 0.1
                        , Joint black 0
                        , Link  black 0.1
                        ])
                , (Point2 0.1 0.15, Vector2 0.1 0, Vector2 0 0.1)
                )            
              , Just ( [3/2 * pi, 1/2 * pi], [1, -1] )
              )
            , ( ( (0  , [ Link  black 0.1
                        , Joint black 0
                        , Link  black 0.1
                        , Joint black 0
                        , Link  black 0.1
                        ])
                , (Point2 0.1 0.15, Vector2 0.1 0, Vector2 (-0.1) 0.1)
                )            
              , Just ( [3/2 * pi, 1/2 * pi], [1, 1] )
              )          
            ]


harmonica :: TestCase
harmonica = ctc 0 1 [ pi * 1/2,  pi, 0.1
                    , pi * 1/2, -pi, 0.1
                    , pi * 3/2, -pi, 0.1
                    , pi * 3/2,  pi, 0.1
                    ]

ctc :: Float -> Float -> [Float] -> TestCase
ctc foot l1 xs = 
  let angles =          map fst $ filter (\(x, i) -> i `mod` 3 == 0) $ zip xs [0..]
      speeds =          map fst $ filter (\(x, i) -> i `mod` 3 == 1) $ zip xs [0..]
      links  = (l1 :) $ map fst $ filter (\(x, i) -> i `mod` 3 == 2) $ zip xs [0..]
  in createTestCase foot (buildArm links angles) speeds

buildArm :: [Float] -> [Float] -> Arm
buildArm [l] [] = [Link black l]
buildArm (l : ls) (j : js) = Link black l : Joint black j : buildArm ls js

-- use forward kinematics to create a test case
createTestCase :: Float -> Arm -> [Float] -> TestCase
createTestCase foot arm speeds =
  let angles = getJointAngles arm
      arm' = setJointAngles (repeat 0) arm
      ps   = evaluate arm
      p    = last ps
      q    = last $ init ps
      bat  = ClosedLineSegment (p :+ ()) (q :+ ())
      m    = origin .+^ (chop $ toVec $ average [p, q])
      n    = chop $ normalVectorOfSegment bat
      vs   = motionVelocity speeds arm
      v    = chop $ toVec $ average [origin .+^ last vs, origin .+^ (last . init) vs]
  in (((foot, arm'), (m, n, v)), Just (angles, speeds))

normalVectorOfSegment :: Floating r => LineSegment 2 () r -> Vector 2 r
normalVectorOfSegment s = transformBy (rotation $ pi / 2) $ (s ^. end ^. core) .-. (s ^. start ^. core)

getJointAngles :: Arm -> [Float]
getJointAngles arm = map (\(Joint _ x) -> x) $ filter isJoint arm

setJointAngles :: [Float] -> Arm -> Arm
setJointAngles m (l@(Link _ _) : arm) = l : setJointAngles m arm
setJointAngles (x : xs) (Joint c a : arm) = Joint c x : setJointAngles xs arm
setJointAngles _ arm = arm


average :: (Functor t, Foldable t, Arity d, Fractional r) => t (Point d r) -> Point d r
average ps = origin .+^ foldr1 (^+^) (fmap toVec ps) ^/ realToFrac (length ps)

chop :: RealFrac r => Vector 2 r -> Vector 2 r
chop v = toVec $ snap 6 $ origin .+^ v

snap :: RealFrac r => Int -> Point 2 r -> Point 2 r
snap k (Point2 x y) = Point2 (snapC k x) (snapC k y)
snapC :: RealFrac r => Int -> r -> r
snapC k x = fromInteger (round ((10 ^ k) * x)) / (10 ^ k)

-- TESTING


testPlan :: Planner -> TestCase -> IO ()
testPlan f ((xarm, conf), answer) = do
  putStrLn $ replicate 80 '-' 
  putStrLn $ "test case:" 
  putStr   $ writePlanInput xarm conf
  result <- f xarm conf
  when (compareResults answer result) $ do
    putStrLn $ "correct result:"
    putStrLn $ writePlanOutput result
  when (not $ compareResults answer result) $ do
    putStrLn $ "incorrect result:"
    putStrLn $ writePlanOutput result
    putStrLn $ "should have been"
    putStrLn $ writePlanOutput answer
    putStrLn $ "Disclaimer: Currently this script only tests whether your answer is equal to the model answer. However, some questions have multiple correct answers. So, your answer might still be correct."

testPlans :: Planner -> IO ()
testPlans f = do
  prepareAll
  prepare SocketPlayer.player 
  sequence_ $ map (testPlan f) testcases

compareResults :: TestOutput -> TestOutput -> Bool
compareResults result1 result2 = compareValues (values result1) (values result2)

compareValues :: [Float] -> [Float] -> Bool
compareValues xs ys | length xs /= length ys = False
                    | otherwise              = and $ zipWith compareValue xs ys

compareValue :: Float -> Float -> Bool
compareValue x y = abs (x - y) < treshold 

treshold :: Float
treshold = 0.0001

values :: TestOutput -> [Float]
values Nothing = []
values (Just (angles, speeds)) = angles ++ speeds