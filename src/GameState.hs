module GameState where

import qualified Data.Map as M
import Data.List (sort)

newtype Characters = Characters String deriving (Eq, Show)

newtype Current = Current Int deriving (Eq, Show)

newtype Answer = Answer Char deriving (Eq, Show)

newtype Score = Score (M.Map Char (Int, Int)) deriving (Eq, Show)


data GState = GState 
              { characters :: Characters
              , current :: Current 
              , answer :: Maybe Answer
              , score :: Score
              } deriving (Eq, Show)
-- invariant: current < length characters


handleAnswer :: Answer -> GState -> GState
handleAnswer (Answer c) gs =
  if c == goal then gs { current = Current (curr + 1)
                       , answer = Nothing
                       , score = right goal sc }
               else gs { answer = Just (Answer c)
                       , score = wrong goal sc }
  where
    Characters chrs = characters gs
    Current curr = current gs
    sc = score gs
    goal = chrs !! curr

gameOver :: GState -> Bool
gameOver (GState (Characters cs) (Current n) _ _) = n >= length cs

newGame :: GState
newGame = GState (Characters "aoeu") (Current 0) Nothing (emptyScore "aoeu")

generateGame :: String -> IO GState
generateGame cs = pure $
  GState
  (Characters cs)
  (Current 0)
  Nothing
  (emptyScore cs)

uniqueChars :: String -> String
uniqueChars = uniqueFromSort . sort
  where
    uniqueFromSort [] = []
    uniqueFromSort [x] = [x]
    uniqueFromSort (x:y:xs) = if x == y
      then uniqueFromSort (y:xs)
      else x : uniqueFromSort (y:xs)

emptyScore :: String -> Score
emptyScore cs =
  Score $ M.fromList $ zip (uniqueChars cs) (repeat (0,0))

wrong :: Char -> Score -> Score
wrong c (Score m) = Score $ M.update bump2 c m
  where bump2 (a,b) = Just (a, b+1)

right :: Char -> Score -> Score
right c (Score m) = Score $ M.update bump1 c m
  where bump1 (a,b) = Just (a+1, b)