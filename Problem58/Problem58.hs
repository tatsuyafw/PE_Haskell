-- Project Euler / Problem58

type Layer = Int


divisors :: Int -> [Int]
divisors n = n : [x | x <- [1..u], n `mod` x == 0]
    where u = n `div` 2
             
isPrime :: Int -> Bool
isPrime n = divisors n == [n, 1]

primeNumAtLayer :: Layer -> Int
primeNumAtLayer l = length . filter isPrime $ nList
    where
      nList = take 4 $ iterate (subtract (base-1)) baseSquare
      baseSquare = base ^ 2
      base = 2 * l + 1

ratio :: (Int, Int) -> Float
ratio (n, d) = fromIntegral n / fromIntegral d

--problem58 = head . dropWhile (> 0.1) $ ratioList
problem58 = take limit $ primeNumList
    where
      ratioList = tail . take limit . map ratio . scanl f (0, 1) $ primeNumList
      limit = 200
      primeNumList = map primeNumAtLayer [1..]
      f (n, m) l = (n + l, m + 4)