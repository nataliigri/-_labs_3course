import Data.List (nub)

countUniqueElements :: Eq a => [a] -> Int
countUniqueElements = length . nub

-- Тест 1: Порожній список
test1 :: Bool
test1 = countUniqueElements ([] :: [Int]) == 0

-- Тест 2: Список з одним елементом
test2 :: Bool
test2 = countUniqueElements [1] == 1

-- Тест 3: Список з дубльованими елементами
test3 :: Bool
test3 = countUniqueElements [1, 2, 3, 1, 2, 4, 5, 3, 6] == 6

-- Тест 4: Список з усіма елементами різними
test4 :: Bool
test4 = countUniqueElements [1, 2, 3, 4, 5, 6] == 6

-- Запуск тестів і виведення результатів
main :: IO ()
main = do
    let list = [1, 2, 3, 7, 2, 8, 4, 5, 3, 6]
    putStrLn $ "У списку " ++ show list ++ " " ++ show (countUniqueElements list) ++ " різних елементів."
    putStrLn $ "Тест 1: " ++ show test1
    putStrLn $ "Тест 2: " ++ show test2
    putStrLn $ "Тест 3: " ++ show test3
    putStrLn $ "Тест 4: " ++ show test4
