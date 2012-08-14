-- Project Euler / Problem55

isPalindromic :: Integer -> Bool
isPalindromic n = nStr == reverse nStr
    where nStr = show n

reverseAndAdd :: Integer -> Integer
reverseAndAdd n = n + rNum
    where rNum = read . reverse $ show n

isLychrel :: Integer -> Bool
isLychrel n = not . any isPalindromic. take limit $ reverseAndAddList
    where
      reverseAndAddList = tail . iterate reverseAndAdd $ n
      limit = 50 - 1

problem55 = length . filter isLychrel $ [1..limit]
    where limit = 10000 - 1
