-- Project Euler / Problem9

problem9 = [a * b * c | c <- nList, b <- [1..c], let a = 1000 - c - b,
                             a < b && a > 0, isPythagorean a b c]
    where nList = [1..1000]

isPythagorean a b c = a^2 + b^2 == c^2
