{-# LANGUAGE DeriveGeneric #-}

module GameState where

import Control.Monad.State
import Test.QuickCheck.Arbitrary
import Data.List (sort)
import System.Random (RandomGen, randomRs)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.Map as M

newtype Characters = Characters String deriving (Eq, Show)

newtype Current = Current Int deriving (Eq, Show)

newtype Answer = Answer Char deriving (Eq, Show)

newtype Score = Score (M.Map Char (Int, Int)) deriving (Eq, Show, Generic)

instance Serialize Score

data GState = GState 
              { characters :: Characters
              , current :: Current 
              , answer :: Maybe Answer
              , score :: Score
              } deriving (Eq, Show)
-- invariant: current < length characters

instance Arbitrary GState where
  arbitrary = do
    s <- arbitrary
    return $ emptyGame s (uniqueChars s)

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

handleAnswerState :: Answer -> State GState ()
handleAnswerState a = state $ \g -> ((), handleAnswer a g)

gameOver :: GState -> Bool
gameOver (GState (Characters cs) (Current n) _ _) = n >= length cs

newGame :: GState
newGame = GState (Characters "aoeu") (Current 0) Nothing (emptyScore "aoeu")

generateGame :: RandomGen g => g -> Int -> String -> GState
generateGame g size cs =
  let uniquecs = uniqueChars cs
      n = length uniquecs
      goals = map (\i -> cs !! i) $ take size $ randomRs (0,n-1) g
  in  emptyGame goals uniquecs

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

emptyGame :: String -> String -> GState
emptyGame goals cs =
  GState (Characters goals) (Current 0) Nothing (emptyScore cs)

wrong :: Char -> Score -> Score
wrong c (Score m) = Score $ M.update bump2 c m
  where bump2 (a,b) = Just (a, b+1)

right :: Char -> Score -> Score
right c (Score m) = Score $ M.update bump1 c m
  where bump1 (a,b) = Just (a+1, b)

combineScores :: Score -> Score -> Score
combineScores (Score a) (Score b) = Score $ M.unionWith addPointwise a b
  where
    addPointwise (a,b) (c,d) = (a+c, b+d)

isPerfect :: Score -> Bool
isPerfect (Score m) = M.foldr singlePerfect True m
  where singlePerfect _ False = False
        singlePerfect (_,0) True = True
        singlePerfect _ True = False

gameInvariant :: GState -> Bool
gameInvariant (GState (Characters cs) (Current n) _ _) = length cs > n
