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

main :: IO ()
main = quickCheck propPerfectGame
