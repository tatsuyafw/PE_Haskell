-- Project Euler / Problem20

import Data.Char

problem20 = sum $ map digitToInt $ show $ factorial 100

factorial :: Integer -> Integer
factorial n = product [1..n]
