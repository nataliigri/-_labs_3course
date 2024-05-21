import Test.HUnit
import Data.List (nub)

-- Функція, що знаходить позиції входжень елемента у список
findPositions :: Eq a => a -> [a] -> [Int]
findPositions _ [] = [] -- якщо список пустий, повертаємо пустий список позицій
findPositions x xs = [i | (i, y) <- zip [0..] xs, y == x]

-- Функція, що знаходить позиції входжень елементів першого списку у другий список
findPositionsList :: Eq a => [a] -> [a] -> [[Int]]
findPositionsList [] _ = [] -- якщо перший список пустий, повертаємо пустий список
findPositionsList _ [] = [] -- якщо другий список пустий, повертаємо пустий список
findPositionsList xs ys = map (\x -> if x `elem` ys then findPositions x ys else [])  (nub xs)

test1 = TestCase $ assertEqual "Test 1" [[0,3,6],[1,4],[2,5]] (findPositionsList [1,2,3,2,1] [1,2,3,1,2,3,1])
test2 = TestCase $ assertEqual "Test 2" [[1,3,5],[0,2],[4,6]] (findPositionsList [2,1,3] [1,2,1,2,3,2,3])
test3 = TestCase $ assertEqual "Test 3" [[],[0],[1,2],[3]] (findPositionsList [5,1,2,3] [1,2,2,3])
test4 = TestCase $ assertEqual "Test 4" [[0,3,4],[1,5,6],[2,7]] (findPositionsList [1,2,3] [1,2,3,1,1,2,2,3])
test5 = TestCase $ assertEqual "Test 5" [] (findPositionsList [1,2,3] [])
test6 = TestCase $ assertEqual "Test 6" [] (findPositionsList [] [1,2,3])
tests = TestList [test1, test2, test3, test4, test5, test6]

main :: IO ()
main = do
    let list1 = [1, 2, 3, 2, 1]
        list2 = [1, 2, 3, 1, 2, 3, 1]
    putStrLn "Перший список"
    print list1 
    putStrLn "Другий список"
    print list2
    putStrLn "Result"
    print $ findPositionsList list1 list2
    putStrLn "Running tests..."
    _ <- runTestTT tests
    return ()
