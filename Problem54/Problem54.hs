-- Project Euler / Problem54

import Data.List
data Hand =
    HC | -- High Card
    OP | -- One Pair
    TP | -- Two Pair
    TK | -- Three of a Kind
    ST | -- Straight
    FL | -- Flush
    FH | -- Full House
    FK | -- Four of a Kind
    SF | -- Straight Flush
    RF   --- Royal Flush
    deriving (Eq, Show)
            
data Value = TWO | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace deriving (Eq, Show, Ord, Enum)
type Kind = Char
data Card = Card Value Kind deriving (Eq, Show)
type Player = (Card, Card, Card, Card, Card)

value :: Card -> Value
value (Card m _ ) = m
values :: Player -> [Value]
values (as, bs, cs, ds, es) = [value as, value bs, value cs, value ds , value es]
kind :: Card -> Kind
kind (Card _ k) = k
kinds :: Player -> [Kind]
kinds (as, bs, cs, ds, es) = [kind as, kind bs, kind cs, kind ds, kind es]

sameKind :: [Kind] -> Bool
sameKind []  = True
sameKind [x] = True
sameKind (x1:x2:xs) = if x1 == x2
                      then sameKind (x2:xs)
                      else False

toRF :: Player -> Maybe Hand
toRF ps = if isRF
          then Just RF
          else Nothing
    where 
      isRF = (sameKind $ kinds ps)
             && (sort $ values ps)== rs
                 where rs = [Ten, Jack, Queen, King, Ace]
                            
isSF :: Player -> Bool
isSF ps =
    (sameKind $ kinds ps)
    && if s >= Jack
       then False
       else ms == [s..(last ms)]
    where
      s = head ms
      ms = sort $ values ps
toSF :: Player -> Maybe (Hand, Value)
toSF ps = if isSF ps
          then Just (SF, hi)
          else Nothing
              where
                hi = last . sort $ values ps
                                
isFK :: Player -> Bool
isFK ps = any gtFour . map (\xs -> (length xs, head xs)) $ group ms
    where
      gtFour (n, _) = n >= 4
      ms = sort $ values ps
toFK :: Player -> Maybe (Hand, Value, Value)
toFK ps = if isFK ps
          then Just (FK, v1, v2)
          else Nothing
              where
                v1 = snd (xs !! 1)
                v2 = snd (xs !! 0)
                xs = sort . map (\xs -> (length xs, head xs)) $ group ms
                ms = sort $ values ps

isFH :: Player -> Bool
isFH ps = twoAndThree . map (\xs -> (length xs, head xs)) $ group ms
    where 
      twoAndThree xs = (map (\ys -> fst ys) $ sort xs) == [2, 3]
      ms = sort $ values ps
toFH :: Player -> Maybe (Hand, Value, Value)
toFH ps = if isFH ps
          then Just (FH, v1, v2)
          else Nothing
              where
                v1 = snd (xs !! 1)
                v2 = snd (xs !! 0)
                xs = sort . map (\xs -> (length xs, head xs)) $ group ms
                ms = sort $ values ps

isFL :: Player -> Bool
isFL ps = sameKind $ kinds ps
toFL :: Player -> Maybe (Hand, Value, Value, Value, Value, Value)
toFL ps = if isFL ps
          then Just (FL, ms !! 4, ms !! 3, ms !! 2, ms !! 1, ms !! 0)
          else Nothing
              where
                ms = sort $ values ps

isST :: Player -> Bool
isST ps = ms == [(head ms)..(last ms)]
    where
      ms = sort $ values ps
toST :: Player -> Maybe (Hand, Value, Value, Value, Value, Value)
toST ps = if isST ps
          then Just (ST, ms !! 4, ms !! 3, ms !! 2, ms !! 1, ms !! 0)
          else Nothing
              where
                ms = sort $ values ps

isTK :: Player -> Bool
isTK ps = oneAndOneAndThree . map (\xs -> (length xs, head xs)) $ group ms
    where
      oneAndOneAndThree xs = (map (\xs -> fst xs) $ sort xs) == [1, 1, 3]
      ms = sort $ values ps
toTK :: Player -> Maybe (Hand, Value, Value, Value)
toTK ps = if isTK ps
          then Just (TK, v1, v2, v3)
          else Nothing
              where
                v1 = snd (xs !! 2)
                v2 = snd (xs !! 1)
                v3 = snd (xs !! 0)
                xs = sort . map (\xs -> (length xs, head xs)) $ group ms
                ms = sort $ values ps

isTP :: Player -> Bool
isTP ps = 

problem54 = 1                                
