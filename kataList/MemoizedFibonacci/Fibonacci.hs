module MemoizedFibonacci.Fibonacci (fibonacci) where

fibonacci :: Int -> Integer
fibonacci = (fibs !!)
  where fibs = scanl (+) 0 (1:fibs)