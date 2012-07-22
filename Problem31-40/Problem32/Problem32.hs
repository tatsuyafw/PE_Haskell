-- Project Euler / Problem32

import Data.List
import Control.Applicative

problem32 = sum $ nub $ l1 ++ l2
    where l1 = [n * m | n <- oneDigitList, m <- fourDigitsList, isUnusual n m]
          l2 = [n * m | n <- twoDigitsList, m <- threeDigitsList, isUnusual n m]

oneDigitList = [1..9]
twoDigitsList = [12..98]
threeDigitsList = [123..987]
fourDigitsList = [1234..9876]

isUnusual :: Integer -> Integer -> Bool
isUnusual n m = sort (nStr ++ mStr ++ pStr) == nineDigitsStr
    where pStr = show $ n * m
          nStr = show n
          mStr = show m

nineDigitsStr = "123456789"
