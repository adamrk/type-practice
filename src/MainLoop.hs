module MainLoop where

import System.Random (newStdGen)
import System.Environment (getArgs)
import Data.Serialize (encode, decode)
import System.IO (hSetBuffering, BufferMode(..), stdin)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import GameState
import Render

gameLoop :: GState -> IO Score
gameLoop gs = do
  input <- getChar
  putStr clearScreen
  let newState = handleAnswer (Answer input) gs
  putStr $ printGame newState
  if gameOver newState
    then putStrLn "\nGood Job!"
         >> putStrLn (printScore $ score newState)
         >> (return $ score newState)
    else gameLoop newState

mainGame :: IO ()
mainGame = do
  savedFile <- safeHead <$> getArgs
  overallScore <- case savedFile of
    Just f -> decode <$> BS.readFile f
    Nothing -> return . Right . Score $ M.fromList []
  case overallScore of
    Right sc -> putStrLn "Saved Scores:" >> (putStrLn $ printScore sc)
    Left _ -> return ()
  initialState <- characterPrompt
  hSetBuffering stdin NoBuffering
  putStr clearScreen
  putStr $ printGame initialState
  newScore <- gameLoop initialState
  let newOverallScore = case overallScore of
        Right sc -> combineScores sc newScore
        Left _ -> newScore
  case savedFile of
    Just f -> BS.writeFile f (encode newOverallScore)
    Nothing -> return ()
  mainGame

characterPrompt :: IO GState
characterPrompt = do
  putStrLn "enter characters to practice:"
  characters <- getLine
  putStrLn "enter game size:"
  size <- read <$> getLine
  randomGen <- newStdGen
  return $ generateGame randomGen size characters

safeHead :: [a] -> Maybe a
safeHead = foldr (\x acc -> Just x) Nothing