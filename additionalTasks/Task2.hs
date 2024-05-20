import Data.Char (intToDigit)
import Test.HUnit

-- Функція для перетворення числа в двійковий рядок
toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse (toBinary' n)
  where
    toBinary' 0 = ""
    toBinary' x = let (q, r) = x `divMod` 2
                  in intToDigit r : toBinary' q

-- Функція для знаходження передостанньої цифри двійкового запису
penultimateBinaryDigit :: Int -> Maybe Char
penultimateBinaryDigit n
    | length binRep < 2 = Nothing  -- Якщо двійковий запис менше 2 цифр
    | otherwise         = Just (binRep !! 1)  -- Передостання цифра
  where
    binRep = reverse (toBinary n)

-- Тести
test1 = TestCase (assertEqual "Test for number 16 (binary: 10000)" (penultimateBinaryDigit 16) (Just '0'))
test2 = TestCase (assertEqual "Test for number 4 (binary: 100)" (penultimateBinaryDigit 4) (Just '0'))
test3 = TestCase (assertEqual "Test for number 2 (binary: 10)" (penultimateBinaryDigit 2) (Just '1'))
test4 = TestCase (assertEqual "Test for number 1 (binary: 1)" (penultimateBinaryDigit 1) Nothing)

-- Функція для запуску тестів з покращеним виводом
runTests :: IO ()
runTests = do
    putStrLn "Running tests..."
    results <- runTestTT $ TestList [TestLabel "Test 1" test1,
                                     TestLabel "Test 2" test2,
                                     TestLabel "Test 3" test3,
                                     TestLabel "Test 4" test4]
    if errors results == 0 && failures results == 0
        then putStrLn "All tests passed successfully."
        else putStrLn "Some tests failed."

main :: IO ()
main = do
    -- Приклад використання функції
    let number = 18  -- Приклад числа, 10010
    let penultimateDigit = penultimateBinaryDigit number
    case penultimateDigit of
        Just digit -> putStrLn $ "Передостання цифра двійкового запису: " ++ [digit]
        Nothing -> putStrLn "Число має менше двох цифр у двійковому запису"
    
    -- Запуск тестів
    runTests
