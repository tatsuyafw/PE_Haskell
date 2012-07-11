-- Project Euler / Problem15

problem15 = factorial 40 `div` ((factorial 20) ^ 2)
    where factorial n = product [1..n]
