module MainLoop where

import System.Random (newStdGen)
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
    then putStrLn "\nGood Job!"
         >> putStrLn (printScore newState)
         >> mainGame
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
  putStrLn "enter game size:"
  size <- read <$> getLine
  randomGen <- newStdGen
  return $ generateGame randomGen size characters
