import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import BinaryGap
import OddOccurrencesInArray
import Data.Bits

main :: IO ()
main = hspec $ do
  describe "binaryGap" $ do
    it "of    7 equals to 0" $ do
      binaryGapInt 7 `shouldBe` 0
      
    it "of   15 equals to 0" $ do
      binaryGapInt 15 `shouldBe` 0
      
    it "of    9 equals to 2" $ do
      binaryGapInt 9 `shouldBe` 2
      
    it "of  529 equals to 4" $ do
      binaryGapInteger 529 `shouldBe` 4
      
    it "of 1041 equals to 5" $ do
      binaryGap (1041::Integer) `shouldBe` 5
      
  describe "OddOccurrencesInArray" $ do
    it "2" $ do
      oddOccurrencesInArrayInteger [1, 1, 2] `shouldBe` 2

    it "3" $ do
      oddOccurrencesInArrayInt [1, 3, 2, 1, 2] `shouldBe` 3

    it "4" $ do
      oddOccurrencesInArray [3::Integer, 1, 3, 2, 4, 1, 2] `shouldBe` 4

    it "7" $ do
      oddOccurrencesInArray [9::Integer, 3, 9, 3, 9, 7, 9] `shouldBe` 7

    it "-1" $ do
      (
        let
          bigList = [1,1000,2000000000] 
        in
          oddOccurrencesInArrayInteger (bigList ++ bigList ++ [-1])
        ) `shouldBe` (-1)
