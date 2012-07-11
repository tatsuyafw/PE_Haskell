-- Project Euler / Problem25

problem25 = fst $ head $ dropWhile f $ zip [0..] fibs
    where f (_, n) = (< 1000) $ length $ show n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)