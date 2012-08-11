-- Project Euler / Problem53

import Data.List

problem53 = length [(n, r)| n <- [1..100], r <- [1..n], combination n r > 10 ^ 6]

combination :: (Integral a) => a -> a -> a
combination n r = factorial n `div` (factorial r * factorial (n-r))

factorial :: (Integral a) => a -> a
factorial n = product [2..n]
