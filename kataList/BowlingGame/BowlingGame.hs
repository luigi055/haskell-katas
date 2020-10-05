module BowlingGame.BowlingGame (getScore) where

import Data.Char (digitToInt, isDigit)

replaceChar :: Char -> String -> String
replaceChar c = filter (/=c)

getScore :: String -> Int
getScore = processScore . replaceChar '|'

parseBall :: Char -> Int
parseBall b
  | b == 'X' || b == '/' = 10
  | b == '-' = 0
  | otherwise = digitToInt b

processScore :: String -> Int
processScore [] = 0
processScore (s1:[]) = parseBall s1
processScore (s1:'X':[]) = 0
processScore (s1:s2:[]) = parseBall s1 + processScore [s2]
processScore (_:'/':s3:[]) = 10 + parseBall s3
processScore xs@(s1:s2:s3:ss)
  | s1 == 'X' && s3 == '/' = parseBall s1 + parseBall s3 + processScore (s3:ss)
  | s1 == 'X' = parseBall s1 + parseBall s2 + parseBall s3 + processScore (s2:s3:ss)
  | s2 == '/' = parseBall s2 + parseBall s3 + processScore (s3:ss)
  | otherwise = parseBall s1 + processScore (s2:s3:ss)