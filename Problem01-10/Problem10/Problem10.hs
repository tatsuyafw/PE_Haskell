-- Project Euler / Problem10

problem10 = sum $ takeWhile (< 10^6 * 2) primes

primes :: [Integer]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]
