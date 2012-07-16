-- Project Euler / Problem18

main = problem18

problem18 = interact f
    where
      f   = (++"\n"). show . head . f' . reverse . f''
      f'  =  foldl1 (\acc xs -> zipWith (+) (maxAjustEntry acc) xs)
      f'' = map str2Ints . lines

str2Ints :: String -> [Int]
str2Ints = map read . words

-- [1, 2, 3, 2, 4] -> [2, 3, 3, 4]
maxAjustEntry :: [Int] -> [Int]
maxAjustEntry xs = zipWith (max) xs $ tail xs
