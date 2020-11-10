module CalculateString.CalculateString (add) where

import Data.Char (isDigit)
import Data.List (groupBy, isPrefixOf)
import Data.Function (on)

add :: String -> Int
add numbers
  | numbers == "" = 0
  | otherwise     = sum . parseNumbers $ numbers

parseNumbers :: String -> [Int]
parseNumbers = map (\x -> read x :: Int) . filter (/=",") . groupBySymbols . showNumbers
  where groupBySymbols :: String -> [String]
        groupBySymbols = groupBy ((==) `on` (not . isDigit))
        showNumbers :: String -> String
        showNumbers "" = ""
        showNumbers [a] = a:""
        showNumbers (x:y:xs)
          | x == '-' && isDigit y = x:y:showNumbers xs
          | isDigit x = x:showNumbers (y:xs)
          | (not . isDigit $ x) && (y == '-' || isDigit y) = ',':showNumbers (y:xs)
          | otherwise = showNumbers (y:xs)