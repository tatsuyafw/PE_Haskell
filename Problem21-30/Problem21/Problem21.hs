-- Project Euler / Problem21

problem21 = sum [x | x <- [1..10000], isAmicable x]

isAmicable :: Int -> Bool
isAmicable n = n /= na && n == nb
    where
      nb = sum $ divisors na
      na = sum $ divisors n

divisors :: Int -> [Int]
divisors n = [x | x <- [1..u], n `mod` x == 0]
    where u = n `div` 2
