-- Project Euler / Problem11

data Point = Point Int Int deriving (Show, Eq)
type Map = [[Int]]

main = problem11
       
problem11 = do
  contents <- getContents
  let m = map f $ lines contents
  print m

ds = [down, right, ldown, rdown]

f :: String -> [Int]      
f = map read . words

mapX :: Map -> Int
mapX (x:xs) = length x
mapY :: Map -> Int
mapY xs = length xs

atP :: Point -> Map -> Int
atP (Point x y) m = if x < 0 || x >= mapX m || y < 0 || y >= mapY m
                    then 0
                    else m !! y !! x

productAjust4Point :: Point -> (Point -> Point) -> Map -> Int
productAjust4Point p f m = productPoint (ajust4 p f) m

productPoint :: [Point] -> Map -> Int
productPoint [] _ = 1
productPoint (p:ps) m = atP p m * productPoint ps m
    
ajust4 :: Point -> (Point -> Point) -> [Point]
ajust4 p f = take 4 $ iterate f p
             
up (Point x y) = Point x (y - 1) -- not used
down (Point x y) = Point x (y + 1)
left (Point x y) = Point (x - 1) y -- not used
right (Point x y) = Point (x + 1) y
ldown (Point x y) = Point (x - 1) (y + 1)
rdown (Point x y) = Point (x + 1) (y + 1)
