module MorseCode.Utils (splitString) where

import Data.List (groupBy)
import Data.Function (on)

splitString :: Char -> String -> [String];
splitString c = filter (/= [c]) . groupBy ((==) `on` (==c))