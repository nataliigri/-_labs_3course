import Test.Hspec
import qualified Task12 as T

main :: IO ()
main = hspec $ do
    describe "keepSquares" $ do
        it "повертає пустий список" $
            T.keepSquares [] `shouldBe` ([] :: [Int])

        it "повертає елемент з першої позиції зі списку з трьох елементів" $
            T.keepSquares [-2, 3, 5] `shouldBe` [-2]
            
        it "повертає перший і четвертий елемент зі списку з чотирьох елементів" $
            T.keepSquares [1, 4, 9, 16] `shouldBe` [1, 16]

        it "повертає коректний результат для рандомного списку" $ do
            randomList <- T.generateRandomIntList 10 (1, 100)
            let expected = T.keepSquares randomList
            T.keepSquares randomList `shouldBe` expected
