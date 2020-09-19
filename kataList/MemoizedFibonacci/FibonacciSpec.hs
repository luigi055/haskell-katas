module MemoizedFibonacci.FibonacciSpec (spec) where

import Text.Printf (printf)
import Test.Hspec
import MemoizedFibonacci.Fibonacci (fibonacci)

isTheFibonacciIndexOf :: Int -> Integer -> Spec
isTheFibonacciIndexOf index result =
    it (printf "should pass index %i and receive %i as result" index result) $
        fibonacci index `shouldBe` result

spec :: Spec
spec = do
  describe "Testing fibonacci Function" $ do
    1 `isTheFibonacciIndexOf` 1
    2 `isTheFibonacciIndexOf` 1
    3 `isTheFibonacciIndexOf` 2
    4 `isTheFibonacciIndexOf` 3
    5 `isTheFibonacciIndexOf` 5
    6 `isTheFibonacciIndexOf` 8
    60 `isTheFibonacciIndexOf` 1548008755920
    70 `isTheFibonacciIndexOf` 190392490709135
    100 `isTheFibonacciIndexOf` 354224848179261915075