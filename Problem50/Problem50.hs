-- Project Euler / Problem50

import Data.List
import Data.Array

type Index = Int
type Limit = Int

main = print problem50
problem50 = fst . head . sortBy rcomp . filter isPTuple $ nList
    where
      rcomp (p1, l1) (p2, l2) = l2 `compare` l1
      isPTuple (p, l) = isPrime ! p

nList = concat . map (scanl f (0, 0)) . map (scanl1WithLimit (+) limit) . tails $ primeList
        where f (s, l) x = (x, l + 1)

limit = 10^6

scanl1WithLimit :: (Int -> Int -> Int) -> Limit -> [Int] -> [Int]
scanl1WithLimit _ _ [] = []
scanl1WithLimit f l (x:xs) = scanlWithLimit f x l xs

scanlWithLimit :: (Int -> Int -> Int) -> Int -> Limit -> [Int] -> [Int]
scanlWithLimit _ _ _ []     = []
scanlWithLimit f q l (x:xs)
    | f q x > l = [q]
    | True = q : (scanlWithLimit f (f q x) l xs)

isPrime :: Array Int Bool
isPrime = aList // pList
    where
      aList = listArray (0, limit) $ repeat False
      pList = primeList >>= (\x -> [(x, True)]) -- prime list

primeList = takeWhile (< limit) $ primes

primes :: [Int]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]
