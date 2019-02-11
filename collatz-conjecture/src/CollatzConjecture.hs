module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz number
    | number <= 0 = Nothing
    | otherwise = Just $ toInteger
                       $ length
                       $ takeWhile (> 1)
                       $ iterate nextCollatz number
    where nextCollatz x
            | even x = x `quot` 2
            | otherwise = x * 3 + 1
