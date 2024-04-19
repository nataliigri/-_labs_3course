import Test.Hspec
import qualified Task21 as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "sieve" $ do
        it "should generate prime numbers correctly" $ do
            take 10 T.primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

    describe "splitByPrimes" $ do
        it "should split the list correctly based on prime numbers" $ do
            let primesList = [2, 3, 5, 7, 11]
                inputList = [1..20]
                expectedResult = [[1],[2],[3,4],[5,6],[7,8,9,10]]
            T.splitByPrimes primesList inputList `shouldBe` expectedResult

        it "should handle empty lists" $ do
            T.splitByPrimes [] [1, 2, 3] `shouldBe` []

        it "should handle empty input list" $ do
            T.splitByPrimes [2, 3, 5] [] `shouldBe` []

        it "should handle large input lists" $ do
            let primesList = take 1000 T.primes
                inputList = [1..10000]
            length (T.splitByPrimes primesList inputList) `shouldBe` 1000
