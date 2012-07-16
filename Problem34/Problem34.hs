-- Project Euler / Problem34

import Data.Char

problem34 = sum $ filter isCurious [3..limit]

limit = (* 7) $ factorial 9

isCurious n = n == (sum $ map (factorialList!!) nList)
    where nList = map digitToInt $ show n

factorialList = [factorial n | n <- [0..9]]

factorial n = product [2..n]
