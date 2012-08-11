-- Project Euler / Problem51
-- This program is very dirty :(

import Data.List
import Data.Maybe
type Len = Int


problem51 = head . filter isPrime . createNumList . map g . sortBy comp. zip' . head . filter f . concat . map (maxLenPrime sixDigitPrimes) $ sequ
    where
      g (ch, _) = ch
      comp (f1, s1) (f2, s2) = s1 `compare` s2
      zip' (_, n, indices) = zip (show n) indices ++ zip (repeat '*') (ts \\ indices)
      makeNum (len, i, xs) = 1
      f (len, i, xs) = len >= 8

createNumList :: String -> [Int]
createNumList str = map read [replace str '*' d | d <- ['0'..'9']]
    where
      replace [] _ _ = []
      replace (x:xs) t ch = if t == x
                          then ch : replace xs t ch
                          else x  : replace xs t ch

ts = [0..5]

sequ :: [[Int]]
sequ = subsequences [1..4] >>= (\x -> [x ++ [5]])

maxLenPrime :: [Int] -> [Int] -> [(Len, Int, [Int])]
maxLenPrime primes ss = f mList
    where
      f list = map (\xs -> (length xs, fromJust $ head xs, ss)) list
      mList = group . sort . filter (/= Nothing) $ filterElems ss primes::[[Maybe Int]]

sixDigitPrimes = dropWhile (<10^5) . takeWhile (<10^6) $ primes

filterElems :: [Int] -> [Int] -> [(Maybe Int)]
filterElems _ [] = []
filterElems fil (x:xs) = f x : filterElems fil xs
            where
              f x = read' . mySlice fil $ show x
              read' m = fmap read m

mySlice :: (Eq a) => [Int] -> [a] -> Maybe [a]
mySlice ss xs = if sameElem $ map (xs !!) rs
                then Just (map (xs !!) ss)
                else Nothing
    where
      rs = ts \\ ss
      ts = [0..(length xs - 1)]

isPrime n = divisor n == [1, n]

divisor n = [x | x <- [1..u], n `mod` x == 0] ++ [n]
    where u = n `div` 2


sameElem :: (Eq a) => [a] -> Bool
sameElem []  = True
sameElem [x] = True
sameElem (x1:x2:xs)
    | x1 /= x2  = False
    | otherwise = sameElem (x2:xs)

primes :: [Int]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]
