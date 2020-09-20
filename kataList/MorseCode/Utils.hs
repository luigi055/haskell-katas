module MorseCode.Utils (splitString, trim) where

import Data.List (groupBy)
import Data.Function (on)
import Data.Char (isSpace)

splitString :: Char -> String -> [String];
splitString c = filter (/= [c]) . groupBy ((==) `on` (==c))


trim :: String -> String
trim = dropFromTailWhile isSpace . dropWhile isSpace
  where dropFromTailWhile _ [] = []
        dropFromTailWhile p items
          | p (last items) = dropFromTailWhile p $ init items
          | otherwise      = items
