import System.IO
import Data.List (words, find)
import Control.Exception (throw, Exception)

type State = String
type Symbol = Char
type Transition = (State, Symbol, State)

data FiniteAutomaton = FiniteAutomaton {
    states :: [State],
    alphabet :: [Symbol],
    transitions :: [Transition],
    startState :: State,
    finalStates :: [State]
} deriving Show

-- Define a custom exception for parse errors
newtype ParseException = InvalidTransitionFormat String
    deriving Show

instance Exception ParseException

-- Function to parse a single transition from a string
parseTransition :: String -> Transition
parseTransition str =
    case words str of
        [start, [symbol], end] -> (start, symbol, end)
        _                      -> throw (InvalidTransitionFormat str)

-- Function to read an automaton from a file
readAutomaton :: FilePath -> IO FiniteAutomaton
readAutomaton filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
        alphabet = map head (words (head linesOfFile))
        states = words (linesOfFile !! 1)
        startState = linesOfFile !! 2
        finalStates = words (linesOfFile !! 3)
        transitionLines = filter (not . null) (drop 4 linesOfFile)
        transitions = map parseTransition transitionLines
    return FiniteAutomaton {
        states = states,
        alphabet = alphabet,
        transitions = transitions,
        startState = startState,
        finalStates = finalStates
    }

acceptWord :: FiniteAutomaton -> [Symbol] -> State -> Bool
acceptWord automaton [] state = state `elem` finalStates automaton
-- Якщо слово закінчилося, перевіряємо, чи поточний стан є фінальним
acceptWord automaton (s:ss) state =
  any (\(_, _, nextState) -> acceptWord automaton ss nextState) applicableTransitions
  -- Функція acceptWord перевіряє, чи допускається слово (список символів) автоматом, починаючи з поточного стану
  where
    applicableTransitions = filter (\(fromState, symbol, _) -> fromState == state && symbol == s) (transitions automaton)
    -- Знаходимо переходи, які відповідають поточному стану і символу

findXXX :: FiniteAutomaton -> [Symbol] -> Maybe ([Symbol], [Symbol])
findXXX automaton x =
  let xAccepted = acceptWord automaton x (startState automaton) in
    if xAccepted && acceptWord automaton (x ++ x) (startState automaton) && acceptWord automaton (x ++ x ++ x) (startState automaton)
      then Just (x, x ++ x ++ x)
      else Nothing

main :: IO ()
main = do
  automaton <- readAutomaton "automaton.txt"
  -- print automaton
  putStrLn "Введіть слово X:"
  x <- getLine  -- Користувач вводить слово
  let result = findXXX automaton x
  case result of
    Just (x, xx) -> putStrLn $ "Слово X: " ++ show x ++ " і слово XXX: " ++ show xx
    Nothing      -> putStrLn "Cлово XXX не приймається автоматом"

-- ghc -o task3 task3.hs
