-- Project Euler / Problem12

import Data.List

main = print problem12

problem12 = fst $ head $ dropWhile (\s -> snd s < 500) $ map (\x -> (x, divisorsNum x)) triangleNums

triangleNums = scanl1 (+) [1..]

divisorsNum :: Int -> Int
divisorsNum = product . map (succ . length) . group . factorization

factorization :: Int -> [Int]
factorization 1 = []
factorization n = x : factorization (n `div` x)
    where x = factors n !! 1

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
