-- Project Euler / Problem31

import Data.Array
type IntegerArray = Array Integer Integer

problem31 = (!200) $ foldr update dpAry coins

coins = [1, 2, 5, 10, 20, 50, 100, 200]

update :: Integer -> IntegerArray -> IntegerArray
update n ary = foldl f ary (indices ary)
    where f ary i = if i - n < 0
                    then ary
                    else ary // [(i, m)]
                        where m = (ary ! i) + (ary ! (i-n))

dpAry = listArray (0, 200) (1 : [0, 0..])