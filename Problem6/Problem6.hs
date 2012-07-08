-- Project Euler / Problem6

problem6 = squareOfSum nList - sumOfSquare nList
    where nList = [1..100]

sumOfSquare :: [Integer] -> Integer
sumOfSquare = foldl (\acc x -> acc + x^2) 0

squareOfSum :: [Integer] -> Integer
squareOfSum = (^2) . foldl (+) 0
