module Render where

import GameState
import Data.List ( sortBy, foldl' )
import qualified Data.Map as M

printGame :: GState -> String
printGame (GState (Characters cs) (Current n) a _) =
  test ++ "\n" ++ indicator ++ "\n" ++ previous ++ lastAnswer ++ "\n"
  where
    test = "\x1b[32m" ++ take n cs ++ "\x1b[36;1m" ++ drop n cs ++ "\x1b[0m"
     --    ^ to green                  ^ to bright cyan             ^ normal
    indicator = replicate n ' ' ++ "\x1b[36;1m^\x1b[0m" -- bright cyan
    previous = "\x1b[32m" ++ take n cs ++ "\x1b[0m" -- green
    lastAnswer = case a of
      Just (Answer a) -> "\x1b[31;1m" ++ [a] ++ "\x1b[0m"
      Nothing -> ""       -- ^ bright red          ^ normal

printScore :: GState -> String
printScore (GState _ _ _ (Score sc)) =
  " cor inc per\n" ++ foldl' (\acc s -> singleScore s ++ acc) "" sortedScores
  where
    compScores (a,b) (c,d) =
      compare (ratio2double c (c+d)) (ratio2double a (a+b))
    sortedScores = sortBy (\x y -> compScores (snd x) (snd y)) $ M.toList sc
    singleScore (k, (a,b)) = [k] ++ "  " ++ show a ++ "  " ++
      show b ++ "  " ++ show (100 * ratio2double a (a+b)) ++ "%\n"

ratio2double :: Int -> Int -> Double
ratio2double a b = if b == 0 then 0 else fromIntegral a / fromIntegral b

clearScreen :: String
clearScreen = "\x1b[2J\x1b[f"
