-- Project Euler / Problem28

problem28 = spiralSum 1001

spiralSum :: Integer -> Integer
spiralSum 1 = 1
spiralSum n = foldl f (n * n) [1..3] + spiralSum (n-2)
    where f acc m = acc + (n * n) - (n - 1) * m
