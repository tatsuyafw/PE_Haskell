-- Project Euler / Problem37

problem37 = sum $ take 11 $ filter isInterestingPrime $ filter f [11, 13..]

isInterestingPrime :: Int -> Bool
isInterestingPrime n = all isPrime lList && all isPrime rList
    where
      lList = [read $ take x nStr | x <- [2..len]]::[Int]
      rList = [read $ drop x nStr | x <- [0..len-2]]::[Int]
      len = length nStr
      nStr = show n
                 
f :: Int -> Bool
f n = not $ last nStr `elem` ['0', '1', '4', '6', '8', '9']
      || head nStr `elem` ['1', '4', '6', '8', '9']
    where nStr = show n
            
isPrime :: Int -> Bool
isPrime n = divisors n == [1, n]

divisors :: Int -> [Int]
divisors n = [x | x <- [1..u], n `mod` x == 0] ++ [n]
    where u = n `div` 2
            
primes :: [Integer]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]