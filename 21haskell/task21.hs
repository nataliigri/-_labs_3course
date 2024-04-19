module Task21 where

import Data.List (partition)
import System.Random (newStdGen, randomRs)

-- Функція для генерації простих чисел за допомогою решета Ератосфена
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- Генеруємо нескінченний список простих чисел
primes :: [Integer]
primes = sieve [2..]

-- Функція, яка розбиває список за заданими простими числами
splitByPrimes :: [Integer] -> [Integer] -> [[Integer]]
splitByPrimes _ [] = []
splitByPrimes [] _ = []
splitByPrimes (p:ps) xs = let
    (smaller, rest) = partition (< p) xs
    in smaller : splitByPrimes ps rest

-- Функція для генерації рандомного списку цілих чисел
generateRandomIntList :: Int -> (Integer, Integer) -> IO [Integer]
generateRandomIntList n range = do
    gen <- newStdGen
    return $ take n (randomRs range gen)

-- Головна функція для демонстрації
main :: IO ()
main = do
    let n = 26
        range = (1, 50)
        numPrimes = 9 -- Число простих чисел для розбиття списку
    randomList <- generateRandomIntList n range
    putStrLn "Початковий список:"
    print randomList
    let primeList = take numPrimes primes  -- Вибірка перших numPrimes простих чисел
    let result = splitByPrimes primeList randomList
    putStrLn "Список після розбиття за простими числами:"
    print result
