module LeapYear (isLeapYear) where

divided :: Integer -> Integer -> Bool
divided year n =  year `rem` n == 0

isLeapYear :: Integer -> Bool
isLeapYear year = (year `divided` 4)
                    && not (year `divided` 100)
                    || (year `divided` 400)
