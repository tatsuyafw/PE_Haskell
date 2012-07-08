-- Project Euler / Problem4

problem4 = maximum [x * y | x <- nList, y <- [x..999], isPalindromic (x * y)]
    where nList = [100..999]

isPalindromic :: Integer -> Bool
isPalindromic n = s == reverse s
    where s = show n
