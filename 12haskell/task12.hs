module Task12 where

import System.Random (randomRs, newStdGen)

-- Функція, яка залишає елементи у позиціях, що відповідають квадратам цілих чисел
keepSquares :: [a] -> [a]
keepSquares xs = [x | (x, i) <- zip xs [1..], isSquare i]
    where
        isSquare :: Int -> Bool
        isSquare n = sqrt (fromIntegral n) == fromIntegral (floor (sqrt (fromIntegral n)))

-- Функція, яка генерує список випадкових цілих чисел у заданому діапазоні та зазначеній довжині
generateRandomIntList :: Int -> (Int, Int) -> IO [Int]
generateRandomIntList n range = do
    gen <- newStdGen
    return $ take n (randomRs range gen)

-- Приклад використання:
main :: IO ()
main = do
    let n = 50
        range = (1, 100)
    randomList <- generateRandomIntList n range
    putStrLn "Початковий список:"
    print randomList
    putStrLn "Список з елементами у позиціях, що відповідають квадратам цілих чисел:"
    print (keepSquares randomList)

