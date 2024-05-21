import Data.List (find, intercalate)

-- Визначення типів
data Symbol = B | A deriving (Eq, Show)
type State = Int
type Transition = (State, Symbol, State)

-- Список переходів автомата
transitions :: [Transition]
transitions = [(0, B, 1), (0, A, 2), (1, B, 3), (1, A, 1), (2, B, 2), (2, A, 3), (3, B, 1), (3, A, 3)]

-- Початковий стан
startState :: State
startState = 0

-- Фінальні стани
finalStates :: [State]
finalStates = [3]

-- Перевірка, чи приймається слово автоматом
acceptWord :: [Symbol] -> State -> Bool
acceptWord [] state = state `elem` finalStates
acceptWord (s:ss) state = any (\(_, _, nextState) -> acceptWord ss nextState) applicableTransitions
  where
    applicableTransitions = filter (\(fromState, symbol, _) -> fromState == state && symbol == s) transitions

-- Генерація всіх можливих слів заданої довжини
generateWords :: Int -> [[Symbol]]
generateWords 0 = [[]]
generateWords n = [s:word | s <- [B, A], word <- generateWords (n - 1)]

-- Пошук слова X та XXX, які приймаються автоматом
findXAndXXX :: Int -> Int -> Maybe (String, String)
findXAndXXX minLen maxLen = findXAndXXX' minLen
  where
    findXAndXXX' len
      | len > maxLen = Nothing
      | otherwise = case find (\x -> acceptWord x startState && acceptWord (x ++ x ++ x) startState) (generateWords len) of
                      Just x  -> Just (wordToString x, wordToString (x ++ x ++ x))
                      Nothing -> findXAndXXX' (len + 1)

-- Перетворення списку символів у рядок
wordToString :: [Symbol] -> String
wordToString = intercalate "" . map show

-- Головна функція
main :: IO ()
main = do
   let result = findXAndXXX 2 5
   case result of
    Just (x, xx) -> putStrLn $ "Знайдено слово X: " ++ show x ++ " та слово XXX: " ++ show xx
    Nothing      -> putStrLn "Немає таких слів"
{- 

    -- Тест 1
   let result1 = findXAndXXX 0 2
   putStrLn "Тест 1:"
   case result1 of
     Just (x, xxx) -> putStrLn $ "Очікувано: Знайдено слово X: \"BB\" та слово XXX: \"BBBBBB\"\nОтримано: Знайдено слово X: " ++ x ++ " та слово XXX: " ++ xxx
     Nothing       -> putStrLn "Очікувано: Знайдено слово X: \"BB\" та слово XXX: \"BBBBBB\"\nОтримано: Слова X та XXX не знайдено"
   putStrLn ""
  
  -- Тест 2
  let result2 = findXAndXXX 3 5
   putStrLn "Тест 2:"
  case result2 of
    Just (x, xxx) -> putStrLn $ "Очікувано: Знайдено слово X: \"BBA\" та слово XXX: \"BBABBABBA\"\nОтримано: Знайдено слово X: " ++ x ++ " та слово XXX: " ++ xxx
    Nothing       -> putStrLn "Очікувано: Знайдено слово X: \"BBA\" та слово XXX: \"BBABBABBA\"\nОтримано: Слова X та XXX не знайдено"
  putStrLn ""
  
  -- Тест 3
  let result3 = findXAndXXX 9 10
  putStrLn "Тест 3:"
  case result3 of
    Just (x, xxx) -> putStrLn $ "Очікувано: Знайдено слово X: \"BBBBBBA\" та слово XXX: \"BBBBBBABBBBBBABBBBBBA\"\nОтримано: Знайдено слово X: " ++ x ++ " та слово XXX: " ++ xxx
    Nothing       -> putStrLn "Очікувано: Знайдено слово X: \"BBBBBBBBA\" та слово XXX: \"BBBBBBBBABBBBBBBBABBBBBBBBA\"\nОтримано: Слова X та XXX не знайдено"
  putStrLn ""
  
  -- Тест 4
  let result4 = findXAndXXX 1 1
  putStrLn "Тест 4:"
  case result4 of
    Just (x, xxx) -> putStrLn $ "Очікувано: Слова X та XXX не знайдено\nОтримано: Знайдено слово X: " ++ x ++ " та слово XXX: " ++ xxx
    Nothing       -> putStrLn "Очікувано: Слова X та XXX не знайдено\nОтримано: Слова X та XXX не знайдено"
-}
