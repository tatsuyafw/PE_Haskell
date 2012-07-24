-- Project Euler / Problem45

problem45 = hexa $ head $ filter isOK [144..]

hexa n = n * (2 * n - 1)

isOK :: Int -> Bool
isOK h = and [isSquare t, isSquare p, t' `mod` 2 == 0, p' `mod` 6 == 0]
    where
      p' = (+1) $ truncate $ sqrt $ fromIntegral p
      p  = 1 + 12 * (4 * h * h - 2 * h)
      t' = (1 `subtract`) $ truncate $ sqrt $ fromIntegral t
      t  = 1 + 4 * (4 * h * h - 2 * h)

isSquare :: Int -> Bool
isSquare n = n == (truncate $ sqrt $ fromIntegral n) ^ 2
