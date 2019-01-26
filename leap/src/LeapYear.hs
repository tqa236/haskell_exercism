module LeapYear (isLeapYear) where

not_divided :: Integer -> Integer -> Bool
not_divided year n =  year `rem` n /= 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | not_divided year 4 = False
  | not_divided year 100 = True
  | not_divided year 400 = False
  | otherwise = True
