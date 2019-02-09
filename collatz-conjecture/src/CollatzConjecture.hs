module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz number
    | number <= 0 = Nothing
    | number == 1 = Just 0
    | number `mod` 2 == 0 = maybeAdd $ collatz (number `div` 2)
    | otherwise = maybeAdd $ collatz (number * 3 + 1)
    where maybeAdd num = sum <$> sequence [Just 1, num]
