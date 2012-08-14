-- Project Euler / Problem56

import Data.Char

strToDigits :: String -> [Int]
strToDigits xs = map digitToInt xs

sumDigits :: Integer -> Int
sumDigits n = sum . strToDigits $ nStr
    where nStr = show n

problem56 = maximum . map sumDigits $ nList
    where
      nList = [a ^ b | a <- [1..limit], b <- [1..limit]]
      limit = 100 - 1
