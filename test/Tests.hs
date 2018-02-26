import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import BinaryGap (binaryGap)
import OddOccurrencesInArray (oddOccurrencesInArray)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "binaryGap" $ do
    it "of    7 equals to 0" $ do
      binaryGap 7 `shouldBe` 0
      
    it "of   15 equals to 0" $ do
      binaryGap 15 `shouldBe` 0
      
    it "of    9 equals to 2" $ do
      binaryGap 9 `shouldBe` 2
      
    it "of  529 equals to 4" $ do
      binaryGap 529 `shouldBe` 4
      
    it "of 1041 equals to 5" $ do
      binaryGap 1041 `shouldBe` 5
      
  describe "OddOccurrencesInArray" $ do
    it "2" $ do
      oddOccurrencesInArray [1, 1, 2] `shouldBe` 2

    it "3" $ do
      oddOccurrencesInArray [1, 3, 2, 1, 2] `shouldBe` 3

    it "4" $ do
      oddOccurrencesInArray [3, 1, 3, 2, 4, 1, 2] `shouldBe` 4
