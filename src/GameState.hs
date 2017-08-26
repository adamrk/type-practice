module GameState where

data Characters = Characters String deriving (Eq, Show)

newtype Current = Current Int deriving (Eq, Show)

newtype Answer = Answer Char deriving (Eq, Show)


data GState = GState 
              { characters :: Characters
              , current :: Current 
              , answer :: Maybe Answer
              } deriving (Eq, Show)
-- invariant: current < length characters


handleAnswer :: Answer -> GState -> GState
handleAnswer (Answer c) gs =
  if c == goal then gs { current = Current (curr + 1), answer = Nothing }
               else gs { answer = Just (Answer c) }
  where
    Characters chrs = characters gs
    Current curr = current gs
    goal = chrs !! curr

newGame :: GState
newGame = GState (Characters "aoeu") (Current 0) Nothing