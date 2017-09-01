import Test.QuickCheck
import Control.Monad.State
import GameState

runGame :: [Answer] -> State GState ()
runGame = foldM (\_ a -> handleAnswerState a) ()

propPerfectGame :: String -> Bool
propPerfectGame s = isPerfect . score $ execState (runGame answers) game
  where
    game = emptyGame s (uniqueChars s)
    answers = map Answer s

propGameInvariant :: GState -> Bool
propGameInvariant = gameInvariant

main :: IO ()
main = do
  quickCheck $ label "perfect games score as expected" propPerfectGame
  quickCheck $ label "game state invariant fails with empty test characters"
    $ expectFailure propGameInvariant
