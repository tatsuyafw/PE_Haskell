-- Project Euler / Problem26

import Data.List
import qualified Data.Set as Set

data Pair = Pair Quotient Remainder deriving (Eq, Show, Ord)
type Quotient = Integer
type Remainder = Integer

problem26 = fst $ foldl1 max' $ zip [1..] $ map f [1..1000]
    where
      max' (n1, ps1) (n2, ps2) = if length ps1 > length ps2
                                 then (n1, ps1)
                                 else (n2, ps2)
      f n = recurringCycle n (Pair 0 1) emptySet

recurringCycle :: Integer -> Pair -> (Set.Set Pair) -> [Pair]
recurringCycle n (Pair _ 0) s = []
recurringCycle n (Pair q r) s
    = if Set.member p s
      then []
      else p : (recurringCycle n p s')
          where s' = Set.insert p s
                p = mydiv n (Pair q r)

emptySet :: Set.Set Pair
emptySet = Set.fromList []

mydiv :: Integer -> Pair -> Pair
mydiv n (Pair q r) = Pair q' r'
    where q' = r * 10 `div` n
          r' = r * 10 `mod` n