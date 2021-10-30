module B3 where


import PingPong.Model
import PingPong.Draw
import PingPong.Draw.Rasterific
import PingPong.Simulation
import PingPong.Simulation.Collision

import Graphics.Gloss (black)

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType( Font, loadFontFile )


import Data.Geometry hiding (init, head, replicate)
import Data.Geometry.Matrix
import Data.Geometry.Transformation

import Data.Ext
import Data.List hiding (intersect)
import Data.Foldable
import Data.Char
import Control.Lens hiding (snoc)

import Convert

import Data.Vinyl.CoRec

import Debug.Trace

import System.Random
import System.IO.Unsafe

import System.Random
import System.Process
import System.Directory


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


-- TESTING ACTION FUNCTION OF SocketPlayer

import qualified PingPong.Player.NativePlayer as NativePlayer
import qualified PingPong.Player.SocketPlayer as SocketPlayer
import qualified PingPong.Player.SoloboloPlayer as SoloboloPlayer


main :: IO ()
main = do
  -- prepare   SoloboloPlayer.player
  testShots SoloboloPlayer.player
  terminate SoloboloPlayer.player





type TestCase = BallState

shot ::  Float -> Float -> Float -> Float -> TestCase
shot x y dx dy = BallState (Point2 x y) (Vector2 dx dy)

testcases :: [TestCase]
testcases = 
  [ 
    shot 0.0 0.7 1.4 0.0
  , shot (-0.7) 0.80 1.10 1.20
  , shot 0.0 0.90 0.53 (-0.80)
  , shot (-1.40) 0.40 1.5 1.6
  , shot (-2.0) 1.5 3.8 (-1.0)
  ]


frameRate :: Float
frameRate = 50

frameCount :: Int
frameCount = 1000

frameDuration :: Float
frameDuration = 1 / frameRate

testShots :: Player -> IO ()
testShots p = do
  prepare p
  results <- sequence $ map (flip runTest p) testcases
  let pics = concat $ map snd results
      scores = map fst results
      n = filter isAlphaNum (name p)
  writeReport n scores
  export pics (n ++ "_testshots")
  terminate p

runTest :: BallState -> Player -> IO (Int, [Drawing PixelRGBA8 ()])
runTest ib ip = do
  let initialState = defState 
                   { p1    = ip
                   , p2    = noPlayer
                   , phase = DuringRally
                   , ball  = ib
                   }
  putStrLn $ "test shot for " ++ name ip
  Right font <- loadFontFile "fonts/lato/Lato-Bold.ttf" 
  (hits, pics) <- record frameCount font initialState
  return (grade $ map snd $ reverse hits, pics)





record :: Int -> Font -> State -> IO ([Hit], [Drawing PixelRGBA8 ()])
record 0 _    st = return (hits st, [])
record _ _    st | afterRally $ phase st = return (hits st, [])
record n font os = do
  (ns  , pic ) <- step font os
  (hits, pics) <- record (n - 1) font ns 
  return $ (hits, pic : pics)

step :: Font -> State -> IO (State, Drawing PixelRGBA8 ())
step font os = do
  let pic = drawState font os
  ns <- update defaultCollisionChecker frameDuration os
  return (ns, pic)

afterRally (AfterRally _) = True
afterRally _ = False


grade :: [Item] -> Int
grade (Bat Opponent : Table Self : Bat Self : Table Opponent : _) = 2
grade (Bat Opponent : Table Self : Bat Self : _) = 1
grade _ = 0


export :: [Drawing PixelRGBA8 ()] -> String -> IO ()
export pics matchName = do
  createDirectoryIfMissing True "recording/frame/"
  putStr "recording"
  sequence_ $ zipWith exportFrame [1..] pics
  putStrLn "done!"
  callCommand $ "ffmpeg -y -r " ++ show frameRate ++ " -f image2 -s 1920x1080 -i recording/frame/%d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p recording/" ++ matchName ++ ".mp4"
  removeDirectoryRecursive "recording/frame/"

exportFrame :: Int -> Drawing PixelRGBA8 () -> IO ()
exportFrame i pic = do
  let white = PixelRGBA8 255 255 255 255
      img = renderDrawing 1920 1080 white pic
  writePng ("recording/frame/" ++ show i ++ ".png") img
  putStr $ frameMark i

frameMark :: Int -> String
frameMark i | i `mod` (60 * round frameRate) == 0 = show $ i `div` round frameRate
            | i `mod` (10 * round frameRate) == 0 = "#" 
            | i `mod` round frameRate == 0 = "|" 
            | otherwise                    = "."


writeReport :: String -> [Int] -> IO ()
writeReport n scores = do
  let cs = unlines $ zipWith rep scores testcases
      report = n ++ " results\n\n" 
            ++ cs
            ++ "\n" ++ show (sum scores) ++ " points out of " ++ show (length scores) ++ " shots"
--  writeFile ("reports/" ++ n ++ "_report.txt") report 
  putStrLn report

rep :: Int -> TestCase -> String
rep n c = unwords $ show n : (lines $ printBallState c)

