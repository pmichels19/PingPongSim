module PingPong.Simulation.Realtime where

import PingPong.Model
import PingPong.Draw.Gloss
import PingPong.Simulation
import PingPong.Simulation.Collision

import System.Exit
import Control.Monad

import Graphics.Gloss (Display (InWindow), Picture, Color, white)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Graphics.Gloss.Data.ViewPort

simulationRate :: Int
simulationRate = 50

windowDisplay :: Display
windowDisplay = InWindow "Window" (1600, 800) (100, 100)


play :: Player -> Player -> IO ()
play = playWithCollision defaultCollisionChecker

playWithCollision :: CollisionChecker -> Player -> Player -> IO ()
playWithCollision checker ip1 ip2 = do
  -- clean up any running python processes
  prepareAll
--  terminate ip1 -- in case still running from previous run
--  terminate ip2 -- in case still running from previous run
  prepare ip1
  prepare ip2
  initialState <- initBeforeGame $ defState {p1 = ip1, p2 = ip2}
  simulateIO windowDisplay
             white
             simulationRate
             initialState
             (return . drawState)
             (realtimeUpdate checker)


realtimeUpdate :: CollisionChecker -> ViewPort -> Float -> State -> IO State
realtimeUpdate checker p f os = do
  ns <- update checker f os
  when (phase ns == GameOver) $ endGame ns
  return ns
    
endGame :: State -> IO ()
endGame st = do
  terminate $ p1 st
  terminate $ p2 st
  exitSuccess
