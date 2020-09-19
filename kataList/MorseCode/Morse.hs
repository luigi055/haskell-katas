module MorseCode.Morse
( Morse
, decodeMorseCode
) where

import MorseCode.Utils (splitString)
import qualified Data.Map as M
import Data.Char (toLower)

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList [('a', ".-"), ('b', "-..."), ('c', "-.-."), ('d', "-.."), ('e', "."), ('f', "..-."), ('g', "--."), ('h', "...."), ('i', ".."), ('j', ".---"), ('k', "-.-"), ('l', ".-.."), ('m', "--"), ('n', "-."), ('o', "---"), ('p', ".--."), ('q', "--.-"), ('r', ".-."), ('s', "..."), ('t', "-"), ('u', "..-"), ('v', "...-"), ('w', ".--"), ('x', "-..-"), ('y', "-.--"), ('z', "--.."), ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----."), ('0', "-----")]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

decodeMorseCode :: String -> String
decodeMorseCode =  map maybeToString . map morseToChar . splitString ' '
  where maybeToString :: Maybe Char -> Char
        maybeToString (Just a) = a
        maybeToString Nothing = ' '