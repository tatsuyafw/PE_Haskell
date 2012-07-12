x-- Project Euler / Problem30

import Data.Char

problem30 = sum $ filter isSurprising [2..limit]
    where
      limit = (9 ^ 5) * 5

isSurprising :: Int -> Bool
isSurprising n = (==n) $ sum $ map ((^5) . digitToInt) $ show n
