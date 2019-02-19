module Transpose (transpose) where

import           Data.List.Split (chunksOf, splitOn)

transpose :: [String] -> [String]
transpose = concatMap splitRow
    where splitRow = chunksOf 1
