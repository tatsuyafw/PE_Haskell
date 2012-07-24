-- Project Euler / Problem43

import Data.List
type Range = (Int, Int)

problem43 = sum $ map read
            $ filter hasProperty $ permutations "9876543210"

hasProperty :: String -> Bool
hasProperty cs = and $ zipWith divisible nList pList
    where
      divisible n m = n `mod` m == 0
      pList = [17, 13, 11, 7, 5, 3, 2]
      nList = map read $ map (`slice` cs) range

range :: [Range]
range = [(x, x+2) | x <- [7, 6..1]]

slice :: Range -> [a] -> [a]
slice (s, e) xs = map (xs!!) [s..e]
