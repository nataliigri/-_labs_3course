module Task11 where

import System.Random (randomRs, newStdGen)
import Data.List (partition)

-- Функція, яка знаходить мінімальний та максимальний елементи у списку
minMax :: Ord a => [a] -> (a, a)
minMax [] = error "Порожній список"
minMax [x] = (x, x)
minMax (x:xs) = foldl (\(min', max') elem' -> (min min' elem', max max' elem')) (x, x) xs

-- Функція, яка переставляє всі елементи, що дорівнюють мінімуму та максимуму в початок та кінець списку відповідно
moveMinMaxToFrontAndEnd :: Ord a => [a] -> [a]
moveMinMaxToFrontAndEnd [] = []
moveMinMaxToFrontAndEnd xs =
    let (minElem, maxElem) = minMax xs
        rest = filter (\x -> x /= minElem && x /= maxElem) xs
    in [minElem] ++ rest ++ [maxElem]


-- Функція, яка генерує список випадкових цілих чисел у заданому діапазоні та зазначеній довжині
generateRandomIntList :: Int -> (Int, Int) -> IO [Int]
generateRandomIntList n range = do
    gen <- newStdGen
    return $ take n (randomRs range gen)

-- Приклад використання:
main :: IO ()
main = do
    let n = 10
        range = (1, 100)
    randomList <- generateRandomIntList n range
    putStrLn "Початковий список:"
    print randomList
    let modifiedList = moveMinMaxToFrontAndEnd randomList
    putStrLn "Список з мінімальним та максимальним елементами у початку та кінці відповідно:"
    print modifiedList
