-- Project Euler / Problem16

import Data.Char

problem16 = sumDigits $ 2^1000
    where sumDigits = foldl (\acc x -> digitToInt x + acc) 0 . show
