-- Project Euler / Problem52

import Data.List

problem52 = head . head . filter allSameDigit . map sixTimesList $ candicateList

allSameDigit :: [Int] -> Bool
allSameDigit []   = True
allSameDigit [x]  = True
allSameDigit (x1:x2:xs) = if (sort $ show x1) == (sort $ show x2)
                          then allSameDigit (x2:xs)
                          else False

sixTimesList :: Int -> [Int]
sixTimesList n = [n * x | x <- [1..6]]

candicateList :: [Int]
candicateList = concat [createList n | n <- [1..7]]
    where createList n = [lower..upper]
              where
                lower = (10 ^ n) `div` 10
                upper = (10 ^ n) `div` 6
