module Grains (square, total) where

square :: Integer -> Maybe Integer
square = Just 2 (^^)

total :: Integer
total = 2 ^ 64 - 1
