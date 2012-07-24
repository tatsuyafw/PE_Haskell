-- Project Euler / Problem42

import Data.Char
import Data.List.Split

main = problem42
problem42 = do
  contents <- fmap (delete '"') getContents
  let words = splitOn "," contents
      triangleWords = filter isTriangle words
  print $ length triangleWords

isTriangle :: String -> Bool
isTriangle xs = (== v) $ head $ dropWhile (< v) triangleNums
    where v = sum $ map ((b `subtract`) . ord) xs
          b = ord 'A' - 1

triangleNums :: [Int]
triangleNums = [func n | n <- [1..]]
    where func n = n * (n + 1) `div` 2

delete :: Char -> String -> String
delete c = filter (/= c)
