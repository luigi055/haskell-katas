module CalculateString.CalculateStringSpec (spec) where

import Test.Hspec
import CalculateString.CalculateString (add)

spec :: Spec
spec = do
  describe "Testing the add function" $ do
    it "should return 0 when pass in an empty string as argument" $ do
      add "" `shouldBe` 0

    it "should return the same number when only one number is passed in " $ do
      add "1" `shouldBe` 1
      add "55" `shouldBe` 55
      add "274" `shouldBe` 274

    it "should return the sum of 2 numbers when 2 arguments are passed in " $ do
      add "1,2" `shouldBe` 3
      add "60,40" `shouldBe` 100

    it "should sum all the values passed " $ do
      add "1,2,3,4" `shouldBe` 10
      add "2,20,200,2000" `shouldBe` 2222

    it "should handle new lines between numbers" $ do
      add "1\n2,3,4\n5" `shouldBe` 15

    it "should change the delimiter if add the change delimiter syntax " $ do
      -- delimiter change syntax = //[delimiter]\n[numbers…]
      add "//;\n1;2;3;4;5" `shouldBe` 15
      add "//$\n1$2$3$4$5" `shouldBe` 15