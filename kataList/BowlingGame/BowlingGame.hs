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
processScore [c1] = parseBall c1
processScore [c1,c2] = parseBall c1 + processScore [c2]
processScore (c1:c2:c3:cs)
  | isSpareWithBonus = ballTwo + ballThree
  | isStrikeWithBonus = ballOne + ballTwo + ballThree
  | isStrikeAndNextSpare = ballOne + ballThree + processScore (c2:c3:cs)
  | isStrike = ballOne + ballTwo + ballThree + processScore (c2:c3:cs)
  | isSpare = ballTwo + ballThree + processScore (c3:cs)
  | otherwise = ballOne + processScore (c2:c3:cs)
    where isStrike = c1 == 'X'
          isSpare = c2 == '/'
          isStrikeAndNextSpare = isStrike && c3 == '/'
          isStrikeWithBonus = isStrike && null cs
          isLastBallStrike = c2 == 'X' && null cs
          isSpareWithBonus = isSpare && null cs
          ballOne = parseBall c1
          ballTwo = parseBall c2
          ballThree = parseBall c3