module Render where

import GameState

printGame :: GState -> String
printGame (GState (Characters cs) (Current n) _) =
  cs ++ "\n" ++ indicator ++ "\n"
  where
    indicator = replicate n ' ' ++ "^"
