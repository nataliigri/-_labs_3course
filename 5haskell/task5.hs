import Data.Maybe (Maybe(..))

n :: Double
n = 3.0 
-- 421 * 3   =  1 263 

-- Завдання 1
-- f2
maybeF2 :: Double -> Maybe Double
maybeF2 x =
  let value = n * x + log x
  in if value <= 0 || isNaN (log x) then Nothing else Just (1 / value)

-- f6
maybeF6 :: Double -> Maybe Double
maybeF6 x =
  let value = x^2 - n * x + 1
  in if value == 0 then Nothing else Just (1 / value)

-- f3
maybeF3 :: Double -> Maybe Double
maybeF3 x =
  let value = log x + sqrt n
  in if value <= 0 || isNaN (log x) then Nothing else Just (1 / value)

-- Завдання 2
-- Функція для обчислення суперпозиції трьох функцій
maybeSuperpositionDo :: Double -> Maybe Double
maybeSuperpositionDo x = do
  fx <- maybeF2 x
  gx <- maybeF6 fx
  maybeF3 gx


maybeSuperpositionWithoutDo :: Double -> Maybe Double
maybeSuperpositionWithoutDo x = maybeF2 x >>= maybeF6 >>= maybeF3

-- Завдання 3
maybeF3Task3 :: (Ord a, Floating a, RealFloat a) => a -> a -> Maybe a
maybeF3Task3 x n
  | isNaN x || isNaN n || x <= 0 || n <= 0 = Nothing
  | otherwise = let value = log x + sqrt n
                in if isNaN value || value <= 0 then Nothing else Just (1 / value)

-- Завдання 4
maybeSuperpositionDoT4 :: Double -> Maybe Double
maybeSuperpositionDoT4  x = do
  fx <- maybeF2 x
  gx <- maybeF6 x
  maybeF3Task3 fx gx

maybeSuperpositionWithoutDoT4 :: Double -> Maybe Double
maybeSuperpositionWithoutDoT4 x =
  maybeF2 x >>= \fx ->
  maybeF6 x >>= \gx ->
  maybeF3Task3 fx gx

main :: IO ()
main = do
  print $ "Testing Task 1..."
  print $ "f2"
  print $ maybeF2 3 -- Just 9.902350653882648e-2
  print $ maybeF2 6 -- Just 5.052607887412869e-2
  print $ maybeF2 0 -- Nothing

  print $ "f6"
  print $ maybeF6 1 -- Just (-1.0)
  print $ maybeF6 4 -- Just 0.2

  print $ "f3"
  print $ maybeF3 2 -- Just 0.4123374688973565
  print $ maybeF3 (-1) -- Nothing
  print $ maybeF3 (1/4) -- Just 2.8922092712088925

  putStrLn "\nTesting Task 2..."
  print $ maybeSuperpositionDo 2
  print $ maybeSuperpositionWithoutDo 2

  print $ maybeSuperpositionDo 4
  print $ maybeSuperpositionWithoutDo 4

  print $ maybeSuperpositionDo 0
  print $ maybeSuperpositionWithoutDo 0

  print $ maybeSuperpositionDo (-3)
  print $ maybeSuperpositionWithoutDo  (-3)
  

  putStrLn "\nTesting Task 3..."
  print $ maybeF3Task3 2 8 
  print $ maybeF3Task3 (-3) 2
  print $ maybeF3Task3 (1/4) 2
  print $ maybeF3Task3 100000 1
  print $ maybeF3Task3 (1/7 + 0.2) 4
  print $ maybeF3Task3 1 8 
  print $ maybeF3Task3 0.5 1 
  

  putStrLn "\nTesting Task 4..."
  print $ maybeSuperpositionDoT4 8  
  print $ maybeSuperpositionWithoutDoT4 8 
  
  print $ maybeSuperpositionDoT4 4
  print $ maybeSuperpositionWithoutDoT4 4
  
  print $ maybeSuperpositionDoT4 0.2 
  print $ maybeSuperpositionWithoutDoT4 0.2
  
  print $ maybeSuperpositionDoT4 1 
  print $ maybeSuperpositionWithoutDoT4 1 
  
  print $ maybeSuperpositionDoT4 0
  print $ maybeSuperpositionWithoutDoT4 0
