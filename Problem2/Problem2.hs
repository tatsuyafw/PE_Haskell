-- Project Euler / Problem2

problem2 = sum $ takeWhile (< 10^6 * 4) [x | x <- fib, even x]

fib :: [Int]
fib = 1 : 1 : zipWith (+) fib (tail fib)
