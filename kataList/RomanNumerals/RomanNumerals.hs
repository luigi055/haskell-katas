module RomanNumerals.RomanNumerals (convertToRoman) where

convertToRoman :: Integer -> String
convertToRoman n
  | n >= 1000 = "M" ++ convertToRoman (n-1000)
  | n >= 900  = "CM"++ convertToRoman (n-900)
  | n >= 500  = "D" ++ convertToRoman (n-500)
  | n >= 400  = "CD"++ convertToRoman (n-400)
  | n >= 100  = "C" ++ convertToRoman (n-100)
  | n >= 90   = "XC"++ convertToRoman (n-90)
  | n >= 50   = "L" ++ convertToRoman (n-50)
  | n >= 40   = "XL"++ convertToRoman (n-40)
  | n >= 10   = "X" ++ convertToRoman (n-10)
  | n >= 9    = "IX"++ convertToRoman (n-9)
  | n >= 5    = "V" ++ convertToRoman (n-5)
  | n >= 4    = "IV"++ convertToRoman (n-4)
  | n >= 1    = "I" ++ convertToRoman (n-1)
  | n == 0    = ""