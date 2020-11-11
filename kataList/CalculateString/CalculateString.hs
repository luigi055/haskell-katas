module CalculateString.CalculateString (add) where

import Data.Char (isDigit)
import Data.List (groupBy, isPrefixOf)
import Data.Function (on)

add :: String -> Int
add numbers
  | not . null $ negativeNumbers = error errorMessage
  | otherwise = sum parsedNumbers
    where parsedNumbers         = parseNumbers numbers
          negativeNumbers       = filter (0>) parsedNumbers
          formattedErrorNumbers = tail . init . show $ negativeNumbers
          errorMessage          = "Negative Numbers like: " ++ formattedErrorNumbers ++ " are not allowed"

parseNumbers :: String -> [Int]
parseNumbers = filter (<=1000) . map (\x -> read x :: Int) . filter (/=",") . groupBy ((==) `on` (==',')) . showNumbers
  where showNumbers :: String -> String
        showNumbers ""  = ""
        showNumbers [a] = a:""
        showNumbers (x:y:xs)
          | x == '-' && isDigit y      = x:y:showNumbers xs
          | isDigit x                  = x:showNumbers (y:xs)
          | (not . isDigit $ x)
            && (y == '-' || isDigit y) = ',':showNumbers (y:xs)
          | otherwise                  = showNumbers (y:xs)