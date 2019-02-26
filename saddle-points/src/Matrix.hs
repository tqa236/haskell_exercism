module Matrix (saddlePoints) where

import           Data.Array (Array, array, assocs, indices)
import           Data.Ix
import           Data.List

saddlePoints :: Ix i => Array i e -> [i]
saddlePoints matrix = [last (indices matrix)]
    where numRow = rangeSize $ indices matrix
          -- numCol = snd $ last $ indices matrix
