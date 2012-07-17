-- Project Euler / Problem39

import Data.List

main  = print problem39    
problem39 = fst $ maximumBy comp $zip [120..] $ map (length . rightTriangles) [120..1000]
            where comp (_, len1) (_, len2) = len1 `compare` len2

rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles p = [(a, b, c) | c <- [l..u], b <- [(c`div`2)..c-1],
                                     let a = p - c - b, a > 0 && a < b, isRightTriangle a b c]
    where u = p `div` 2
          l = p `div` 3 + 1

isRightTriangle :: Int -> Int -> Int -> Bool
isRightTriangle a b c = a^2 + b^2 == c^2
