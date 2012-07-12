-- Project Euler / Problem27

main = print problem27

problem27 = (\(a, b, l) -> a * b) $ foldl1 mycomp nList
    where
      mycomp (a1, b1, l1) (a2, b2, l2) = if l1 > l2 then (a1, b1, l1) else (a2, b2, l2)
      nList = [(a, b, consecutiveLength a b) | b <- bList, a <- [-1000..1000], 1 + a + b > 0]

consecutiveLength a b = length $ consecutive a b

bList = takeWhile (<1000) primes

consecutive :: Integer -> Integer -> [Integer]
consecutive a b = takeWhile isPrime $ map (func a b) [0..]

isPrime n = divisors n == [1, n]

divisors n = [x | x <- [1..u], n `mod` x == 0] ++ [n]
    where u = n `div` 2

func a b n = n * n + a * n + b

primes :: [Integer]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]
