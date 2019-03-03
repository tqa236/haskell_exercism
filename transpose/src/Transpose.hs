module Transpose (transpose) where

import           Data.Char
import qualified Data.List as List

transpose :: [String] -> [String]
transpose matrix = List.transpose equalLengthMatrix
    where maxLength = maximum $ map length matrix
          equalLengthMatrix
            | null matrix = matrix
            | maxLength == length (head matrix) = matrix
            | otherwise = map (\x -> x ++ replicate (maxLength - length x) ' ') matrix
