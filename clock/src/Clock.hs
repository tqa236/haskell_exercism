module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock exactHour exactMinute
    where exactHour = (hour + minute `div` 60) `mod` 24
          exactMinute = minute `mod` 60
          -- The function choices are essential here
          -- div will round the result toward minus infinitive
          -- quot will round the result toward 0
          -- the result of mod has the same sign as the divisor
          -- the result of rem has the same sign as the dividend
          -- (ex: 2 `mod` -3 = -1, 2 `rem` -3 = 2)

toString :: Clock -> String
toString (Clock hour minute) = format hour ++ ":" ++ format minute
  where format n
          | n < 10 = '0' : show n
          | otherwise = show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock hour' minute') =
    fromHourMin (hour + hour') (minute + minute')
