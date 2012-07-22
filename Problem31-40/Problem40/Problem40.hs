-- Project Euler / Problem40

import Data.Char

problem40 = product $ map (digitToInt . dN) $ take 7 $ iterate (*10) 1

dN :: Int -> Char
dN n = (!! n) $ foldr1 (++) $ map show [0..]
