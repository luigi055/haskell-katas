module RomanNumerals.RomanNumeralsSpec (spec) where

import Text.Printf (printf)
import Test.Hspec
import RomanNumerals.RomanNumerals (convertToRoman)

inRomanNumeralIs :: Integer -> String -> Spec
inRomanNumeralIs n s =
    it (printf "should translate %i to %s" n s) $
        convertToRoman n `shouldBe` s

spec :: Spec
spec = do
  describe "Testing convertToRoman Function" $ do
    1 `inRomanNumeralIs` "I"
    4 `inRomanNumeralIs` "IV"
    6 `inRomanNumeralIs` "VI"
    14 `inRomanNumeralIs` "XIV"
    21 `inRomanNumeralIs` "XXI"
    89 `inRomanNumeralIs` "LXXXIX"
    91 `inRomanNumeralIs` "XCI"
    984 `inRomanNumeralIs` "CMLXXXIV"
    1889 `inRomanNumeralIs` "MDCCCLXXXIX" 
    1989 `inRomanNumeralIs` "MCMLXXXIX"
    2019 `inRomanNumeralIs` "MMXIX"