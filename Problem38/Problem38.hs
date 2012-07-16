-- Project Euler / Problem38

import Data.List

problem38 = foldl1 max $ filter isPandigital $ map f nList
    where
      f (n1, n2) = read (show n1 ++ show n2) :: Int
      nList = [(x, x * 2) | x <- [9123..9876]]

isPandigital :: Int -> Bool
isPandigital n = sortedNStr == ['1'..'9']
                 where sortedNStr = sort $ show n
