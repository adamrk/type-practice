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
  gameLoop newState

mainGame :: IO ()
mainGame = do
  hSetBuffering stdin NoBuffering
  putStr $ printGame newGame
  gameLoop newGame