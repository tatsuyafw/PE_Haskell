-- Project Euler / Problem48

problem48 :: Integer
problem48 = read . reverse . take 10 . reverse . show . sum
            . map (\n -> n^n) $ [1..1000]
