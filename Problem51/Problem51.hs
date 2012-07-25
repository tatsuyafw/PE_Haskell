-- Project Euler / Problem51

import Data.List
import Data.Maybe
type Len = Int

problem51 = head . filter f . concat . map (maxLenPrime sixDigitPrimes) $ sequ
    where
      makeNum (len, i, xs) = 1
      f (len, i, xs) = len >= 8

myZip ()
            
sequ :: [[Int]]
sequ = subsequences [1..4] >>= (\x -> [x ++ [5]])
            
--fiveDigitPrimes = dropWhile (<10^4) . takeWhile (<10^5) $ primes

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
              f x = myread . mySlice fil $ show x
              myread (Just a) = (Just $ read a)
              myread Nothing  = Nothing

mySlice :: (Eq a) => [Int] -> [a] -> Maybe [a]
mySlice ss xs = if sameElem $ map (xs !!) rs
                then Just (map (xs !!) ss)
                else Nothing
    where
      rs = filter (\x -> not $ x `elem` ss) ts
      ts = [0..(length xs - 1)]

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
