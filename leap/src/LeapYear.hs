module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year =
  if mod year 4 == 0
  then
    if mod year 100 == 0 && mod year 400 /= 0
    then False
    else True
  else False
