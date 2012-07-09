-- Project Euler / Problem17

import Data.Char

problem17 = sum $ map (length . filter (/= ' ')) $ map numStr [1..1000]

numStr :: Int -> String
numStr n | n == 1000   = "one thousand"
         | n < 20      = below20 !! n
         | n < 100     = tens !! (n `div` 10) ++ " " ++ below20 !! (n `mod` 10)
         | n `mod` 100 == 0 = below20 !! (n `div` 100) ++ " hundred"
         | n < 1000    = below20 !! (n `div` 100) ++ " hundred and " ++ numStr (n `mod` 100)
         | otherwise   = ""

below20 :: [String]
below20 = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
           "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen",
           "eighteen", "nineteen"]

tens :: [String]
tens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
