-- Project Euler / Problem35

import Data.Array

main = print problem35
problem35 = length $ filter isCircularPrime $ primeList

isCircularPrime :: Integer -> Bool
isCircularPrime n = all isPrime $ rotateIntList n

isPrime n = isPrimeArray ! n

isPrimeArray = (// (zip primeList trueList)) $ listArray (0, 10^6) falseList
    where falseList = repeat False
          trueList = repeat True
primeList = takeWhile (< 10^6) primes

primes :: [Integer]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]

rotateIntList :: Integer -> [Integer]
rotateIntList n = [rotateInt r n | r <- [0..len-1]]
    where len = length $ show n

rotateInt :: Int -> Integer -> Integer
rotateInt n m = read $ rotate n $ show m

rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate _ [] = []
rotate n xs = drop n xs ++ take n xs
