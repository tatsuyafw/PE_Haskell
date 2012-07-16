-- Project Euler / Problem14

import Data.List

main = print problem14

problem14 = fst $ maximumBy m $ map (\n -> (n, length $ collatz n)) [1..(10^6)]
    where m (_, n1) (_, n2) = compare n1 n2

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = if even n
            then n : collatz (n `div` 2)
            else n : collatz (3 * n + 1)
