module BinarySearch (find) where

import           Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = findBinary array x 0 (length (indices array) - 1)

findBinary :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
findBinary array x lowerBound upperBound
    | lowerBound > upperBound = Nothing
    | value == x = Just pivot
    | value > x = findBinary array x lowerBound (pivot - 1)
    | otherwise = findBinary array x (pivot + 1) upperBound
    where pivot = (lowerBound + upperBound) `quot` 2
          value = array ! pivot
