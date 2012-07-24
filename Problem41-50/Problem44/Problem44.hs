-- Project Euler / Problem44

type Index = Int

main = print problem44
problem44 = head [penta k - penta j | k <- [3..], j <- [1..k-1], isOK j k]

penta n = (n * (3 * n - 1)) `div` 2

isOK :: Index -> Index -> Bool
isOK j k = isSquare m && m' `mod` 6 == 0 && isSquare l && l' `mod` 6 == 0
    where
      l' = (+1) $ truncate $ sqrt $ fromIntegral l
      l  = 1 + 12 * ((3 * k * k) - k - (3 * j * j) + j)
      m' = (+1) $ truncate $ sqrt $ fromIntegral m
      m  = 1 + 12 * ((3 * j * j) - j + (3 * k * k) - k)

isSquare :: Int -> Bool
isSquare n = n == (truncate $ sqrt $ fromIntegral n) ^ 2
