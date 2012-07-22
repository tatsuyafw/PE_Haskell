-- Project Euler / Problem49

import Data.List

problem49 :: Integer
problem49 = read . (!!1) . map joinInts . concat $ map incSequence sameDigitPrimes
    where
      sameDigitPrimes = [map (\(a, _) -> a) ts | ts <- groupSndEqlTuples,
                                                       length ts >= 3]
      groupSndEqlTuples = groupBy comp' sortedTuples
      comp' (_, b1) (_, b2) = b1 == b2
      sortedTuples = sortBy comp primeTuples
      comp (_, b1) (_, b2) = b1 `compare` b2
      primeTuples = map (\n -> (n, sort $ show n)) fourDgtPrimes

joinInts :: [Int] -> String
joinInts = foldl' (\acc x -> acc ++ show x) ""

incSequence :: [Int] -> [[Int]]
incSequence xs = filter f [ns | ns <- subsequences xs, length ns == 3]
    where f (x:y:z:[]) = z - y == y - x

fourDgtPrimes = takeWhile (<10000) . dropWhile (<1000) $ primes

primes :: [Int]
primes = 2 : f [3] [3, 5..]
    where f (x:xs) ys = let (ps, qs) = span (< x^2) ys
                        in ps ++ f (xs ++ ps) [n | n <- qs, n `mod` x /= 0]
