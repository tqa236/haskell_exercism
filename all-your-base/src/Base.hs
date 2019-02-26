module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | invalidInputDigits /= [] = Left $ InvalidDigit $ head invalidInputDigits
    | otherwise = Right $ reverse $ decToBase outputBase $ baseToDec inputBase $ reverse inputDigits
    where invalidInputDigits = filter (not . (`elem` [0..(inputBase - 1)])) inputDigits

baseToDec :: Integral a => a -> [a] -> a
baseToDec _ []             = 0
baseToDec inputBase (x:xs) = x + inputBase * baseToDec inputBase xs

decToBase :: Integral a => a -> a -> [a]
decToBase _ 0 = []
decToBase outputBase number = number `mod` outputBase : decToBase outputBase (number `div` outputBase)
