module LeapYear (isLeapYear) where

notDivided :: Integer -> Integer -> Bool
notDivided year n =  year `rem` n /= 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | notDivided year 4 = False
  | notDivided year 100 = True
  | notDivided year 400 = False
  | otherwise = True
