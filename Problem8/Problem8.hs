-- Project Euler / Problem8

import Data.Char

main = problem8

problem8 = do
  contents <- getContents
  let line = chomp contents
  let nList = map productStr $ take5 line
  print $ maximum nList

chomp :: String -> String
chomp "" = []
chomp (x:xs) = if x == '\n'
               then chomp xs
               else x : chomp xs

take5 :: [a] -> [[a]]
take5 all@(x:xs) = if length all < 5
                   then []
                   else [take 5 all] ++ take5 xs

productStr :: String -> Int
productStr = product . map (digitToInt)
