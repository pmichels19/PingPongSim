module B1 where

import PingPong
import PingPong.Player
import PingPong.Simulation.Collision

import Data.Geometry hiding (init, head, replicate)
import Data.Ext
import Control.Monad

import System.Random


-- TESTING COLLISION FUNCTION OF NativePlayer

import qualified PingPong.Player.NativePlayer as NativePlayer

main :: IO ()
main = testCollisions NativePlayer.collision 
--main = testCollisions defaultCollisionChecker


-- TEST CASES

type TestInput  = ((Float, Point 2 Float, LineSegment 2 () Float), (Float, Point 2 Float, LineSegment 2 () Float))
type TestOutput = (Maybe (Float, Point 2 Float, Vector 2 Float))
type TestCase   = (TestInput, TestOutput)

testcases :: [TestCase]
testcases = [ ( ( (0  , Point2 0 0, OpenLineSegment (Point2 1 (-1) :+ ()) (Point2 1 1 :+ ()))
                , (0.1, Point2 0 0, OpenLineSegment (Point2 1 (-1) :+ ()) (Point2 1 1 :+ ()))
                )            
              , Nothing
              )
            , ( ( (0  , Point2 0 0, OpenLineSegment (Point2 1 (-1) :+ ()) (Point2 1 1 :+ ()))
                , (0.1, Point2 2 0, OpenLineSegment (Point2 1 (-1) :+ ()) (Point2 1 1 :+ ()))
                )            
              , Just (0.05, Point2 1 0, Vector2 (-20) 0)
              )
            , ( ( (0.42, Point2 0 0, OpenLineSegment (Point2 1 (-1) :+ ()) (Point2 1 1 :+ ()))
                , (0.44, Point2 1 0, OpenLineSegment (Point2 0 (-1) :+ ()) (Point2 0 1 :+ ()))
                )            
              , Just (0.43, Point2 0.5 0, Vector2 (-150) 0)
              )
            , ( ( (100, Point2 0 0, OpenLineSegment (Point2 0 (-1) :+ ()) (Point2 2 1 :+ ()))
                , (101, Point2 2 0, OpenLineSegment (Point2 0 (-1) :+ ()) (Point2 2 1 :+ ()))
                )            
              , Just (100.5, Point2 1 0, Vector2 0 2)
              )
            , ( ( (0  , Point2 0 0, OpenLineSegment (Point2   0  (-1) :+ ()) (Point2 2 1 :+ ()))
                , (100, Point2 0 0, OpenLineSegment (Point2 (-2) (-1) :+ ()) (Point2 0 1 :+ ()))
                )            
              , Just (50, Point2 0 0, Vector2 (-0.02) 0.02)
              )
            ]



testCollision :: CollisionChecker -> TestCase -> IO ()
testCollision f ((state1, state2), answer) = do
  putStrLn $ replicate 80 '-' 
  putStrLn $ "test case:" 
  putStr   $ writeCollisionInput state1 state2
  result <- f state1 state2
  when (compareResults answer result) $ do
    putStrLn $ "correct result:"
    putStrLn $ writeCollisionOutput result
  when (not $ compareResults answer result) $ do
    putStrLn $ "incorrect result:"
    putStrLn $ writeCollisionOutput result
    putStrLn $ "should have been"
    putStrLn $ writeCollisionOutput answer

testCollisions :: CollisionChecker -> IO ()
testCollisions f = do
  sequence_ $ map (testCollision f) testcases

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
values (Just (t, Point2 px py, Vector2 vx vy)) = [t, px, py, vx, vy]