module LeapYear (isLeapYear) where

divided :: Integer -> Integer -> Bool
divided year n =  year `rem` n == 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | divided year 4 = case divided year 100 of
    True  -> divided year 400
    False -> True
  | otherwise = False
