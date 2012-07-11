-- Project Euler / Problem24

import Control.Applicative
import Data.Char

main = print problem24

problem24 = myperms "0123456789" !! 999999

myperms :: String -> [String]
myperms [] = [""]
myperms xs = concat $ zipWith join (strToStrs xs) [myperms ys | ys <- sList]
    where sList = map (deleteAt xs) [0..u]
          u = length xs - 1

-- join "a" ["b", "c"] = ["ab", "ac"]
join :: String -> [String] -> [String]
join xs yss = (++) <$> [xs] <*> yss

-- strToStrs "abc" = ["a", "b", "c"]
strToStrs :: String -> [String]
strToStrs = map (\c -> [c])

-- deleteAt "abc" 1 = "ac"
deleteAt :: [a] -> Int -> [a]
deleteAt [] _ = []
deleteAt (x:xs) 0 = xs
deleteAt (x:xs) n = x : deleteAt xs (n-1)