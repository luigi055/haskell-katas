module CalculateString.CalculateString (add) where

import Data.Char (isDigit)
import Data.List (groupBy, isPrefixOf)
import Data.Function (on)

slash = '/'
newLine = "\n"
comma = ','

calculateSum delimiter strings = parseSum delimiter strings 0
  where parseSum :: Char -> [String] -> Int -> Int
        parseSum _ [] acc = acc
        parseSum d (x:xs) acc
         | x == slash:slash:d:newLine
          || x == [d]
          || x == newLine = parseSum d xs acc
         | otherwise      = parseSum d xs (acc + (read x::Int))

parseDelimiter :: String -> Char
parseDelimiter s
  | (slash:slash:"") `isPrefixOf` s = s !! 2
  | otherwise                       = comma


add :: String -> Int
add numbers
  | numbers == "" = 0
  | otherwise     = calculateSum (parseDelimiter numbers) . groupBy ((==) `on` (not . isDigit)) $ numbers