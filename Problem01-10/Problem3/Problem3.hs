-- Project Euler / Problem3

problem3 = last $ factorization 600851475143

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]

factorization :: Integer -> [Integer]
factorization 1 = []
factorization n = x : factorization (n `div` x)
  where
    x = (factors n) !! 1
