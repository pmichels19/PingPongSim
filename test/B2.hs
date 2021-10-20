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
              , Just ( [3/2 * pi + asin 0.6, pi, asin (-0.6)], [0, 0, 0] )
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