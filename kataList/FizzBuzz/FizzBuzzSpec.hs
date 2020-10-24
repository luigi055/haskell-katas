module FizzBuzz.FizzBuzzSpec (spec) where

import Test.Hspec
import FizzBuzz.FizzBuzz (fizzBuzz)

spec :: Spec
spec = do
  describe "Testing fizzBuzz Function" $ do
    it "should return Left 1" $ do
      fizzBuzz 1 `shouldBe` Left 1

    it "should return Fizz if number is divisible by 3" $ do
      fizzBuzz 3 `shouldBe` Right "Fizz"
      fizzBuzz 12 `shouldBe` Right "Fizz"

    it "should return Buzz if number is divisible by 5" $ do
      fizzBuzz 5 `shouldBe` Right "Buzz"
      fizzBuzz 25 `shouldBe` Right "Buzz"
      fizzBuzz 100 `shouldBe` Right "Buzz"

    it "should return FizzBuzz if number is divisible by 5 and 3" $ do
      fizzBuzz 15 `shouldBe` Right "FizzBuzz"
      fizzBuzz 75 `shouldBe` Right "FizzBuzz"
      fizzBuzz 30 `shouldBe` Right "FizzBuzz"
      fizzBuzz 45 `shouldBe` Right "FizzBuzz"
      fizzBuzz 51 `shouldBe` Right "FizzBuzz"
      fizzBuzz 57 `shouldBe` Right "FizzBuzz"
      fizzBuzz 54 `shouldBe` Right "FizzBuzz"
      fizzBuzz 60 `shouldBe` Right "FizzBuzz"
      fizzBuzz 35 `shouldBe` Right "FizzBuzz"

    it "should return Fizz when the number has a 3" $ do
      fizzBuzz 37 `shouldBe` Right "Fizz"
      fizzBuzz 32 `shouldBe` Right "Fizz"
      fizzBuzz 39 `shouldBe` Right "Fizz"

    it "should return Fizz when the number ends on 3" $ do
      fizzBuzz 13 `shouldBe` Right "Fizz"
      fizzBuzz 73 `shouldBe` Right "Fizz"

    it "should return Buzz when the number has a 5" $ do
      fizzBuzz 56 `shouldBe` Right "Buzz"
      fizzBuzz 58 `shouldBe` Right "Buzz"
      fizzBuzz 55 `shouldBe` Right "Buzz"

    it "should return message when the number is larger than 100" $ do
      fizzBuzz 101 `shouldBe` Right "Larger Than 100"
      fizzBuzz 150 `shouldBe` Right "Larger Than 100"
      fizzBuzz 500 `shouldBe` Right "Larger Than 100"