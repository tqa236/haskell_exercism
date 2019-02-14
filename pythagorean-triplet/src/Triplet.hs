module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = triplets 1 1 (sum - 2)

triplets :: Int -> Int -> Int -> [(Int, Int, Int)]
triplets a b c
    | a > c = []
    | b > c = triplets (a + 1) (a + 1) (b + c - a - 2)
    | a ^ 2 + b ^ 2 == c ^ 2 = (a, b, c) : triplets a (b + 1) (c - 1)
    | otherwise = triplets a (b + 1) (c - 1)
