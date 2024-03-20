-- Файл tests11.hs

import Test.Hspec
import qualified Task11 as T

main :: IO ()
main = hspec $ do
  describe "minMax" $ do
    it "повертає мінімальний та максимальний елементи у списку" $ do
      let list1 = [3, 5, 2, 8, 1]
      T.minMax list1 `shouldBe` (1, 8)

      let list2 = [-10, 0, 15, 7, -5]
      T.minMax list2 `shouldBe` (-10, 15)

  describe "moveMinMaxToFrontAndEnd" $ do
    it "переміщує всі елементи, що дорівнюють мінімуму та максимуму, на початок та кінець списку відповідно" $ do
      let list1 = [3, 5, 2, 8, 1]
      T.moveMinMaxToFrontAndEnd list1 `shouldBe` [1, 3, 5, 2, 8]

      let list2 = [-10, 0, 15, 7, -5]
      T.moveMinMaxToFrontAndEnd list2 `shouldBe` [-10, 0, 7, -5, 15]

    it "preserves the order of non-minimal and non-maximal elements in the list" $ do
      let list1 = [3, 5, 2, 8, 1]
      T.moveMinMaxToFrontAndEnd list1 `shouldSatisfy` (\result -> take 2 result == [1, 3] && drop 2 result == [5, 2, 8])

      let list2 = [-10, 0, 15, 7, -5]
      T.moveMinMaxToFrontAndEnd list2 `shouldSatisfy` (\result -> take 2 result == [-10, 0] && drop 2 result == [7, -5, 15])
