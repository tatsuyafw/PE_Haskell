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
--problem58 = take limit $ primeNumList
--problem58 = until checkRatio (\(n, d) -> )
problem58 = func (0, 1) primeNumList
    where
      func acc (x:xs) = if checkRatio acc
                              then return acc
                              else do
                                print x
                                func (f acc x) xs
      checkRatio t = (ratio t < 0.1) && (ratio t /= 0.0)
      --ratioList = take limit . map ratio . scanr f (0, 1) $ primeNumList
      --ratioList = take limit . map ratio . scanr f (0, 1) $ primeNumList
      --limit = 300
      primeNumList = map primeNumAtLayer [1..]
      f (n, d) x = (n + x, d + 4)
      --primeNumList = map primeNumAtLayer [1..]
      