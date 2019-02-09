module Prime (nth) where

nth :: Int -> Maybe Integer
nth n = Just (nthPrime n)

nthPrime :: Int -> Integer
nthPrime n
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = 3
