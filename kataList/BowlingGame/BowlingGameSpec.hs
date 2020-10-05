module BowlingGame.BowlingGameSpec (spec) where

import BowlingGame.BowlingGame (getScore)
import Test.Hspec
import Text.Printf (printf)

scoresTo :: String -> Int -> Spec
scoresTo frames expected =
    it (printf "should %s return %i" frames expected) $
        getScore frames `shouldBe` expected

spec :: Spec
spec = do
  describe "Testing getScore function" $ do
      ("X|X|X|X|X|X|X|X|X|X||XX" `scoresTo` 300)
      ("9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||" `scoresTo` 90)
      ("-9|-9|-9|-9|-9|-9|-9|-9|-9|9-||" `scoresTo` 90)
      ("-9|9-|-9|-9|9-|-9|-9|9-|-9|-9||" `scoresTo` 90)
      ("5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5" `scoresTo` 150)
      ("54|63|45|36|72|27|18|81|18|54||" `scoresTo` 90)
      ("X|7/|9-|X|-8|8/|-6|X|X|X||81" `scoresTo` 167)
      ("91|91|93|93|92|92|91|21|91|91||" `scoresTo` 99)
      ("91|9-|9-|9-|9-|9-|92|9-|9-|9-||" `scoresTo` 93)
      ("9-|X|8-|9-|9-|9-|9-|9-|9-|9-||" `scoresTo` 98)
      ("--|X|81|--|--|--|--|--|--|--||" `scoresTo` 28)
      ("--|X|8-|1-|--|--|--|--|--|--||" `scoresTo` 27)
      ("9-|8/|--|9-|9-|9-|9-|9-|9-|9-||" `scoresTo` 82)
      ("9-|8/|--|9-|9-|9-|9-|9-|9-|9/||1" `scoresTo` 84)
      ("--|8/|-1|--|--|--|--|--|--|--||" `scoresTo` 11)
      ("--|8/|1-|--|--|--|--|--|--|--||" `scoresTo` 12)