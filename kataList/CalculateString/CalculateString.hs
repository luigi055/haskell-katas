module CalculateString.CalculateString (add) where

import Data.Char (isDigit)
import Data.List (groupBy, isPrefixOf)
import Data.Function (on)

add :: String -> Int
add = sum . parseNumbers

parseNumbers :: String -> [Int]
parseNumbers = map (\x -> read x :: Int) . filter (/=",") . groupBy ((==) `on` (==',')) . showNumbers
  where showNumbers :: String -> String
        showNumbers ""  = ""
        showNumbers [a] = a:""
        showNumbers (x:y:xs)
          | x == '-' && isDigit y = x:y:showNumbers xs
          | isDigit x = x:showNumbers (y:xs)
          | (not . isDigit $ x) && (y == '-' || isDigit y) = ',':showNumbers (y:xs)
          | otherwise = showNumbers (y:xs)