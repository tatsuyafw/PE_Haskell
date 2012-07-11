-- Project Euler / Problem13

import Data.Char

main = problem13

problem13 = interact f
    where f = (++ "\n") . take 10 . show . sum . map read . lines
