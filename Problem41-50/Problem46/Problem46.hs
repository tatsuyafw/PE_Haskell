-- Project Euler / Problem46

problem46 = head $ dropWhile isT $ filter (not . isPrime) [35, 37..]

isT :: Int -> Bool
isT n = or $ map f $ map (`subtract` n) $ lowerPrimeList n
        where f n = n `mod` 2 == 0 && (isSquare $ (n `div` 2))

lowerPrimeList :: Int -> [Int]
lowerPrimeList n = takeWhile (< n) primes

isSquare :: Int -> Bool
isSquare n = n == (truncate $ sqrt $ fromIntegral n) ^ 2

isPrime :: Int -> Bool
isPrime n = divisors n == [1, n]

divisors :: Int -> [Int]
divisors n = [x | x <- [1..u], n `mod` x == 0] ++ [n]
    where u = n `div` 2

primes :: [Int]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]
