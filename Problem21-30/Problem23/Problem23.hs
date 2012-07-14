-- Project Euler / Problem23

main = print problem23

problem23 = sum $ filter (not . isSumOfabundant) [1..28123]

isSumOfabundant n = any f [1..u]
    where
      u   = n `div` 2
      f x = (abundantList !! x) && (abundantList !! (n - x))

abundantList = [x < (sum $ divisors x)| x <- [0..]]

divisors :: Int -> [Int]
divisors n = [x | x <- [1..u], n `mod` x == 0]
    where u = n `div` 2
