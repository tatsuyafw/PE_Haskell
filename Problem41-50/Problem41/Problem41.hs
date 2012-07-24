-- Project Euler / Problem41

import Control.Applicative
import Data.Char

main = print problem41
problem41 = read $ head $ filter (isPrime . read) $ concat $ map myperms [pandigital x | x <- [9,8..1]] :: Int

isPrime :: Int -> Bool
isPrime n = divisors n == [1, n]

divisors n = [x | x <- [1..u], n `mod` x == 0] ++ [n]
    where u = n `div` 2

pandigital :: Int -> String
pandigital 1 = "1"
pandigital 2 = "21"
pandigital n = [c1, c2..'1']
    where c1 = intToDigit n
          c2 = intToDigit (n-1)

myperms :: String -> [String]
myperms [] = [""]
myperms xs = concat $ zipWith join (strToStrs xs) [myperms ys | ys <- sList]
    where sList = map (deleteAt xs) [0..u]
          u = length xs - 1

-- join "a" ["b", "c"] = ["ab", "ac"]
join :: String -> [String] -> [String]
join xs yss = (++) <$> [xs] <*> yss

-- strToStrs "abc" = ["a", "b", "c"]
strToStrs :: String -> [String]
strToStrs = map (\c -> [c])

-- deleteAt "abc" 1 = "ac"
deleteAt :: [a] -> Int -> [a]
deleteAt [] _ = []
deleteAt (x:xs) 0 = xs
deleteAt (x:xs) n = x : deleteAt xs (n-1)
