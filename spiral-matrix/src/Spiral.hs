module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size
    | size == 0 = []
    | size == 1 = [[1]]
    | otherwise = [[1]]
