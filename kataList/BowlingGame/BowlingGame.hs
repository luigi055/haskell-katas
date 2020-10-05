module BowlingGame.BowlingGame (getScore) where

import Data.Char (digitToInt, isDigit)

getScore :: String -> Int
getScore = processScore . filter (/='|')

strikeSymbol = 'X'
spareSymbol  = '/'
emptySymbol  = '-'

parseBall :: Char -> Int
parseBall c
  | c == strikeSymbol || c == spareSymbol = 10
  | c == emptySymbol = 0
  | otherwise = digitToInt c

processScore :: String -> Int
processScore [] = 0
processScore [c1] = parseBall c1
processScore (c1:c2:cs)
  | areLastTwoBalls       = parsedBallOne + processScore [c2]
  | isSpareWithBonus      = parsedBallTwo + parsedBallThree
  | isStrikeWithBonus     = parsedBallOne + parsedBallTwo + parsedBallThree
  | isStrikeAndNextSpare  = parsedBallOne + parsedBallThree + processScore (c2:cs)
  | isStrike c1           = parsedBallOne + parsedBallTwo + parsedBallThree + processScore (c2:cs)
  | isSpare c2            = parsedBallTwo + parsedBallThree + processScore cs
  | otherwise             = parsedBallOne + processScore (c2:cs)
    where isStrike c            = c == strikeSymbol
          isSpare c             = c == spareSymbol
          isStrikeAndNextSpare  = isStrike c1 && (isSpare . head) cs
          isStrikeWithBonus     = isStrike c1 && length cs == 1
          isLastBallStrike      = isStrike c2 && length cs == 1
          isSpareWithBonus      = isSpare c2 && length cs == 1
          areLastTwoBalls       = length cs == 0
          parsedBallOne         = parseBall c1
          parsedBallTwo         = parseBall c2
          parsedBallThree       = parseBall . head $ cs