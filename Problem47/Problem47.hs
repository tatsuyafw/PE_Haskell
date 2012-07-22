-- Project Euler / Problem47

import Data.List

main = print problem47
problem47 = func [x | x <- [1..limit], dstPrimeNum x == 4]
    where
      limit = 10^6

func :: [Int] -> Int
func (v:w:x:y:rest) = if all (==1) [w-v, x-w, y-x]
                      then v
                      else func (w:x:y:rest)

fourConsecutive = all (== 4)

groupN :: Int -> [a] -> [[a]]
groupN n xs
    | length xs < n = []
    | otherwise     = take n xs : groupN n (tail xs)

dstPrimeNum :: Int -> Int
dstPrimeNum n = length $ map (\xs -> head xs) $ group $ factors n

factors :: Int -> [Int]
factors 1 = []
factors n = m : factors (n `div` m)
    where m = divisors n !! 1

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]
    where u = n `div` 2
