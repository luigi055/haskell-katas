module FizzBuzz.FizzBuzz where

fizzBuzz :: Int -> Either Int String
fizzBuzz n
  | n > 100    = Right "Larger Than 100"
  | isFizzBuzz = Right "FizzBuzz"
  | isFizz     = Right "Fizz"
  | isBuzz     = Right "Buzz"
  | otherwise  = Left n
  where isFizz     = n `isDivisibleBy` 3 || n `endsOn` 3 || ((n >= 30) && (n < 40))
        isBuzz     = n `isDivisibleBy` 5 || ((n >= 50) && (n < 60))
        isFizzBuzz = isFizz && isBuzz

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy = (.) (0==) . mod

endsOn :: Int -> Int -> Bool
endsOn = (==) . (`mod` 10)