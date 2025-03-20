module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix =
  [ (r, c)
  | r <- [minRow..maxRow]
  , c <- [minCol..maxCol]
  , let val = matrix ! (r, c)
  , val == maximum (getRow r)
  , val == minimum (getCol c)
  ]
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    getRow r = [matrix ! (r, c) | c <- [minCol..maxCol]]
    getCol c = [matrix ! (r, c) | r <- [minRow..maxRow]]
