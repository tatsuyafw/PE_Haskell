-- Project Euler / Problem33

import Data.List
import Data.Ratio
import Control.Applicative

problem33 = denominator $ foldl f1 (1 % 1) $ filter f2 $ filter f3 nList
    where
      f1 r (d1, n1) = (n1 % d1) * r
      f2 (d, n) = ((fromInteger d) / (fromInteger n)) == delCommonDigitAndDiv d n
      f3 (d, n) = haveCommonDigit d n


nList = [(d, n) | d <- [10..99], n <- [10..(d-1)], d `mod` 10 /= 0, d `mod` 11 /= 0
        , n `mod` 10 /= 0, n `mod` 11 /= 0]

haveCommonDigit :: Integer -> Integer -> Bool
haveCommonDigit n m = or $ (==) <$> nStr <*> mStr
    where nStr = show n
          mStr = show m

delCommonDigitAndDiv :: Integer -> Integer -> Float
delCommonDigitAndDiv n m = nFloat / mFloat
    where nFloat = fromInteger $ deleteDigit n common
          mFloat = fromInteger $ deleteDigit m common
          common = commonDigit n  m

deleteDigit :: Integer -> Char -> Integer
deleteDigit n c = (read $ delete c nStr) :: Integer
    where nStr = show n

commonDigit :: Integer -> Integer -> Char
commonDigit n m = (!! 0) $ head $ filter equalChar $ (concatChar) <$> nStr <*> mStr
    where
      concatChar ch1 ch2 = [ch1, ch2]
      equalChar str = (str !! 0) == (str !! 1)
      nStr = show n
      mStr = show m