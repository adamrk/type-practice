module Render where

import GameState

printGame :: GState -> String
printGame (GState (Characters cs) (Current n) a _) =
  cs ++ "\n" ++ indicator ++ "\n" ++ previous ++ lastAnswer ++ "\n"
  where
    indicator = replicate n ' ' ++ "^"
    previous = take n cs
    lastAnswer = case a of
      Just (Answer a) -> [a]
      Nothing -> ""