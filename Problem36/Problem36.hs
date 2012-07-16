-- Project Euler / Problem36

import Data.Char
import Numeric

problem36 = sum$ [x | x <- [1..limit], isPalinBase10 x, isPalinBase2 $ fromIntegral x]

limit = 10 ^ 6 - 1

isPalinBase10 :: Integer -> Bool
isPalinBase10 n = nStr == reverse nStr
    where nStr = show n

isPalinBase2 :: Int -> Bool
isPalinBase2 n = nStrBase2 == reverse nStrBase2
    where nStrBase2 = showIntAtBase 2 intToDigit n ""
