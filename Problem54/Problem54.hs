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
    deriving (Eq, Show, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace deriving (Eq, Show, Ord, Enum)
data Kind = H | D | S | C deriving (Eq, Show)
data Card = Card Value Kind deriving (Eq, Show)
type Player = (Card, Card, Card, Card, Card)
type Rank = Maybe (Hand, [Value])

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

toRF :: Player -> Rank
toRF ps = if isRF
          then Just (RF, [])
          else Nothing
    where
      isRF = (sameKind $ kinds ps)
             && (sort $ values ps) == rs
                 where rs = [Ten, Jack, Queen, King, Ace]

isSF :: Player -> Bool
isSF ps = (sameKind $ kinds ps)
    && if s >= Jack
       then False
       else ms == [s..(last ms)]
    where
      s = head ms
      ms = sort $ values ps
toSF :: Player -> Rank
toSF ps = if isSF ps
          then Just (SF, [hi])
          else Nothing
              where
                hi = last . sort $ values ps

isFK :: Player -> Bool
isFK ps = any (>=4) . map length $ group ms
    where
      ms = sort $ values ps
toFK :: Player -> Rank
toFK ps = if isFK ps
          then Just (FK, vs)
          else Nothing
              where
                vs = map snd xs
                xs = reverse . sort . map (\xs -> (length xs, head xs)) $ group ms
                ms = sort $ values ps

isFH :: Player -> Bool
isFH ps = twoAndThree . map length $ group ms
    where
      twoAndThree xs = sort xs == [2, 3]
      ms = sort $ values ps
toFH :: Player -> Rank
toFH ps = if isFH ps
          then Just (FH, vs)
          else Nothing
              where
                vs = map snd xs
                xs = reverse . sort . map (\xs -> (length xs, head xs)) $ group ms
                ms = sort $ values ps

isFL :: Player -> Bool
isFL ps = sameKind $ kinds ps
toFL :: Player -> Rank
toFL ps = if isFL ps
          then Just (FL, vs)
          else Nothing
              where
                vs = reverse . sort $ values ps

isST :: Player -> Bool
isST ps = ms == [(head ms)..(last ms)]
    where
      ms = sort $ values ps
toST :: Player -> Rank
toST ps = if isST ps
          then Just (ST, ms)
          else Nothing
              where
                ms = reverse . sort $ values ps

isTK :: Player -> Bool
isTK ps = oneAndOneAndThree . map length $ group ms
    where
      oneAndOneAndThree xs = sort xs == [1, 1, 3]
      ms = sort $ values ps
toTK :: Player -> Rank
toTK ps = if isTK ps
          then Just (TK, xs)
          else Nothing
              where
                xs = reverse . sort . map head $ group ms
                ms = sort $ values ps

isTP :: Player -> Bool
isTP ps = oneAndTwoAndTwo . map length $ group ms
    where
      oneAndTwoAndTwo xs = sort xs == [1, 2, 2]
      ms = sort $ values ps
toTP :: Player -> Rank
toTP ps = if isTP ps
          then Just (TP, [v0])
          else Nothing
              where
                v0 = maximum [v | x <- xs, let (l, v) = x, l == 2]
                v1 = minimum [v | x <- xs, let (l, v) = x, l == 2]
                v2 = minimum . map snd $ xs
                xs = reverse . sort . map (\xs -> (length xs, head xs)) $ group ms
                ms = sort $ values ps

isOP :: Player -> Bool
isOP ps = oneAndOneAndOneAndTwo . map length $ group ms
    where
      oneAndOneAndOneAndTwo xs = sort xs == [1, 1, 1, 2]
      ms = sort $ values ps
toOP :: Player -> Rank
toOP ps = if isOP ps
          then Just (OP, v0 : vs)
          else Nothing
              where
                v0 = snd . head $ xs
                vs = reverse . sort . map snd . tail $ xs
                xs = reverse . sort . map (\xs -> (length xs, head xs)) $ group ms
                ms = sort $ values ps

isHC :: Player -> Bool
isHC ps = noPair . map length $ group ms
    where
      noPair xs = sort xs == [1, 1, 1, 1, 1]
      ms = sort $ values ps
toHC :: Player -> Rank
toHC ps = if isHC ps
          then Just (HC, ms)
          else Nothing
              where
                ms = reverse . sort $ values ps

judgeRank :: Player -> Rank
judgeRank ps = head . dropWhile (== Nothing) . reverse . map (\f -> f ps) $ handList
    where handList = [toHC, toOP, toTP, toTK, toST, toFL, toFH, toFK, toSF, toRF]

compValue :: [Value] -> [Value] -> Ordering
compValue (v1:vs1) (v2:vs2) = if v1 == v2
                               then compValue vs1 vs2
                               else v1 `compare` v2

toHand :: Rank -> Hand
toHand (Just (h, _)) = h

compRank :: Rank -> Rank -> Ordering
compRank r1 r2 = if h1 == h2
                 then compValue (toValue r1) (toValue r2)
                 else h1 `compare` h2
                     where
                       h1 = toHand r1
                       h2 = toHand r2
                       toHand (Just (h, _)) = h
                       toValue (Just (_, vs)) = vs

compPlayer :: Player -> Player -> Ordering
compPlayer p1 p2 = compRank r1 r2
    where
      r1 = judgeRank p1
      r2 = judgeRank p2

readValue :: Char -> Value
readValue c
    | c == '2' = Two
    | c == '3' = Three
    | c == '4' = Four
    | c == '5' = Five
    | c == '6' = Six
    | c == '7' = Seven
    | c == '8' = Eight
    | c == '9' = Nine
    | c == 'T' = Ten
    | c == 'J' = Jack
    | c == 'Q' = Queen
    | c == 'K' = King
    | otherwise = Ace

readKind :: Char -> Kind
readKind c
    | c == 'H'  = H
    | c == 'D'  = D
    | c == 'S'  = S
    | otherwise = C

readCard :: String -> Card
readCard vk = Card (readValue v) (readKind k)
    where
      v = vk !! 0
      k = vk !! 1

readPlayer :: [String] -> Player
readPlayer xs = (c0, c1, c2, c3, c4)
    where
      c0 = readCard (xs !! 0)
      c1 = readCard (xs !! 1)
      c2 = readCard (xs !! 2)
      c3 = readCard (xs !! 3)
      c4 = readCard (xs !! 4)

problem54 = do
  lines <- fmap lines getContents
  let contents = map (splitAt 5 . words) lines
      playerMatches = map (\(ps1, ps2) -> ((readPlayer ps1), (readPlayer ps2))) contents
      battles = map (\(p1, p2) -> compPlayer p1 p2) playerMatches
      player1Wins = length . filter (== GT) $ battles
  print player1Wins

main = problem54
