module MorseCode.MorseCodeSpec (spec) where

import Test.Hspec
import MorseCode.Morse (decodeMorseCode, Morse)
import Text.Printf (printf)

shouldBeTranslatedTo :: Morse -> String -> Spec
shouldBeTranslatedTo morse string =
    it (printf "should translate %s to %s" morse string) $
        decodeMorseCode morse `shouldBe` string

spec :: Spec
spec = do
  describe "Testing Morse Code" $ do
    ".-" `shouldBeTranslatedTo` "a"
    ".- -..." `shouldBeTranslatedTo` "ab"
    ".... . -.--   .--- ..- -.. ." `shouldBeTranslatedTo` "hey jude"
    ".... . .-.. .-.. ---   -- .- .-. .. -. .-" `shouldBeTranslatedTo` "hello marina"