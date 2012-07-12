-- Project Euler / Problem29

import qualified Data.Set as Set

problem29 = Set.size $ foldr Set.insert emptySet nList
    where nList = [a ^ b | a <- [2..100], b <- [2..100]]

emptySet = Set.fromList [] :: Set.Set Integer
