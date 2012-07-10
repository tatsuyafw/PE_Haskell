-- Project Euler / Problem22

import Data.Char
import Data.List
import Text.CSV

problem22 = do
  contents <- parseCSVFromFile fname
  let r (Right a) = a
      csv = sort $ head $ r contents
      res = sum $ map multi $ zip (map alphabeticalOrder csv) [1..]
          where multi (x, y) = x * y
  print res

fname = "names.txt"

alphabeticalOrder :: String -> Int
alphabeticalOrder = foldl (\acc c -> ord' c + acc) 0
    where ord' = (+ 1) . subtract (ord 'A') . ord
