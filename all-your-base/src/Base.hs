module Base (Error(..), rebase) where

import           Data.Either.Combinators (mapBoth)
import           Text.Read               (readEither)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | otherwise = Right $ reverse $ decToBase outputBase $ baseToDec inputBase $ reverse inputDigits
    -- | otherwise = mapBoth (Left InvalidDigit) (Right $ reverse $ decToBase outputBase $ baseToDec inputBase) (readEither (reverse inputDigits))

baseToDec :: Integral a => a -> [a] -> a
baseToDec _ [] = 0
baseToDec inputBase (x:xs) = x + inputBase * baseToDec inputBase xs

decToBase :: Integral a => a -> a -> [a]
decToBase _ 0 = []
decToBase outputBase number = number `mod` outputBase : decToBase outputBase (number `div` outputBase)

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
