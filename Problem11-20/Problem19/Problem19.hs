-- Project Euler / Problem19

import Data.Time
import System.Locale

problem19 = length $ filter (\d -> isFistDay d && isSunday d)[s..e]
    where
      s = fromGregorian 1901 1 1
      e = fromGregorian 2000 12 31

isSunday :: Day -> Bool
isSunday d = formatTime defaultTimeLocale "%u" d == "7"

isFistDay :: Day -> Bool
isFistDay d = formatTime defaultTimeLocale "%d" d == "01"
