-- Project Euler / Problem57

compDigits :: (Integer, Integer) -> Ordering
compDigits (n, m) = nDigitNum `compare` mDigitNum
    where
      nDigitNum = length $ show n
      mDigitNum = length $ show m

expandFunc :: (Integer, Integer) -> (Integer, Integer)
expandFunc (n, d) = (newNumera, newDenomi)
    where
      newNumera = newDenomi + d
      newDenomi = n + d

problem57 = length [n | n <- nList, compDigits n == GT]
    where
      nList = take limit . iterate expandFunc $ (1, 1)
      limit = 1000 - 1
