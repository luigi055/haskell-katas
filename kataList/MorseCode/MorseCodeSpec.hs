module MorseCode.MorseCodeSpec (spec) where

import Test.Hspec
import MorseCode.Morse (decodeMorseCode, encodeMorseCode, Morse)
import Text.Printf (printf)

shouldBeTranslatedTo :: Morse -> String -> Spec
shouldBeTranslatedTo morse string =
    it (printf "should translate %s to %s" morse string) $
        decodeMorseCode morse `shouldBe` string

shouldBeEncodedTo :: String -> Morse -> Spec
shouldBeEncodedTo string morse =
    it (printf "should encode latin alphabet %s to morse code%s" string morse) $
        encodeMorseCode string `shouldBe` morse

spec :: Spec
spec = do
  describe "Testing decodeMorseCode function" $ do
    ".-" `shouldBeTranslatedTo` "a"
    ".- -..." `shouldBeTranslatedTo` "ab"
    "   .- -...        " `shouldBeTranslatedTo` "ab"
    ".-   -..." `shouldBeTranslatedTo` "a b"
    "----- .---- ..--- ...-- ....- ..... -.... --... ---.. ----." `shouldBeTranslatedTo` "0123456789"
    ".... . -.--   .--- ..- -.. ." `shouldBeTranslatedTo` "hey jude"
    ".... . .-.. .-.. ---   -- .- .-. .. -. .-" `shouldBeTranslatedTo` "hello marina"
    "..   -.. --- -. -   -.- -. --- .-- .-.-.-" `shouldBeTranslatedTo` "i dont know."

  describe "Testing encodeMorseCode" $ do
    "a" `shouldBeEncodedTo` ".-"
    "A" `shouldBeEncodedTo` ".-"
    "    a bc  " `shouldBeEncodedTo` ".-   -... -.-."
    "a b" `shouldBeEncodedTo` ".-   -..."
    "Hello World" `shouldBeEncodedTo` ".... . .-.. .-.. ---   .-- --- .-. .-.. -.."
    "hello world" `shouldBeEncodedTo` ".... . .-.. .-.. ---   .-- --- .-. .-.. -.."
    "0123456789" `shouldBeEncodedTo` "----- .---- ..--- ...-- ....- ..... -.... --... ---.. ----."
    "Hello, how are you?" `shouldBeEncodedTo` ".... . .-.. .-.. --- --..--   .... --- .--   .- .-. .   -.-- --- ..- ..--.."
    "I dont know." `shouldBeEncodedTo` "..   -.. --- -. -   -.- -. --- .-- .-.-.-"
