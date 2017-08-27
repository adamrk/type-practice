module Render where

import GameState
import Data.List ( sortBy, foldl' )
import qualified Data.Map as M

printGame :: GState -> String
printGame (GState (Characters cs) (Current n) a _) =
  cs ++ "\n" ++ indicator ++ "\n" ++ previous ++ lastAnswer ++ "\n"
  where
    indicator = replicate n ' ' ++ "^"
    previous = take n cs
    lastAnswer = case a of
      Just (Answer a) -> [a]
      Nothing -> ""

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
