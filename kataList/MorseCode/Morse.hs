module MorseCode.Morse
( Morse
, decodeMorseCode
, encodeMorseCode
) where

import MorseCode.Utils (splitString, trim)
import qualified Data.Map as M
import Data.Char (toLower)
import Data.List (intersperse, groupBy)
import Data.Function (on)

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList [('a', ".-"), ('b', "-..."), ('c', "-.-."), ('d', "-.."), ('e', "."), ('f', "..-."), ('g', "--."), ('h', "...."), ('i', ".."), ('j', ".---"), ('k', "-.-"), ('l', ".-.."), ('m', "--"), ('n', "-."), ('o', "---"), ('p', ".--."), ('q', "--.-"), ('r', ".-."), ('s', "..."), ('t', "-"), ('u', "..-"), ('v', "...-"), ('w', ".--"), ('x', "-..-"), ('y', "-.--"), ('z', "--.."), ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----."), ('0', "-----"), ('.', ".-.-.-"), (',', "--..--"), ('?', "..--.."),(' ', " ")]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup (toLower c) letterToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

encodeMorseCode :: String -> Morse
encodeMorseCode = concat . intersperse " " . concat . mapM charToMorse . trim

decodeMorseCode :: String -> String
decodeMorseCode =  map maybeToString . map morseToChar . splitString ' ' . trim
  where maybeToString :: Maybe Char -> Char
        maybeToString (Just a) = a
        maybeToString Nothing  = ' '