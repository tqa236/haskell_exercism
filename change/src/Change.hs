module Change (findFewestCoins) where

import           Control.Monad

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = Just (findFewestCoins2 target (reverse coins))

findFewestCoins2 :: Integer -> [Integer] -> [Integer]
findFewestCoins2 target coins
    | null coins = []
    | target < head coins = findFewestCoins2 target (tail coins)
    | target >= head coins = findFewestCoins2 (target - head coins) coins ++ [head coins]
    | otherwise = []
