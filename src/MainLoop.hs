module MainLoop where

import System.IO
import GameState
import Render

echoLoop :: IO a
echoLoop = do
  input <- getChar
  putStrLn [input]
  echoLoop

unbufferedEcho :: IO a
unbufferedEcho = do
  hSetBuffering stdin NoBuffering
  echoLoop

gameLoop :: GState -> IO ()
gameLoop gs = do
  input <- getChar
  putStr "\b"
  let newState = handleAnswer (Answer input) gs
  putStr $ printGame newState
  if gameOver newState
    then putStrLn "Good Job!" >> putStrLn (printScore newState)
    else gameLoop newState

mainGame :: IO ()
mainGame = do
  initialState <- characterPrompt
  hSetBuffering stdin NoBuffering
  putStr $ printGame initialState
  gameLoop initialState

characterPrompt :: IO GState
characterPrompt = do
  putStrLn "enter characters to practice:"
  characters <- getLine
  generateGame characters