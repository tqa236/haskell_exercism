module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- Store matrix as a flat vector with dimensions
data Matrix a = Matrix 
    { matrixRows :: Int
    , matrixCols :: Int
    , matrixData :: Vector a
    } deriving (Eq, Show)

cols :: Matrix a -> Int
cols = matrixCols

column :: Int -> Matrix a -> Vector a
column c m = V.generate (matrixRows m) (\r -> matrixData m V.! (r * matrixCols m + c - 1))

flatten :: Matrix a -> Vector a
flatten = matrixData

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList xss@(xs:_) = Matrix numRows numCols (V.fromList $ concat xss)
  where
    numRows = length xss
    numCols = length xs

fromString :: Read a => String -> Matrix a
fromString s = fromList $ map (map read . words) (lines s)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m = Matrix r c (matrixData m)

row :: Int -> Matrix a -> Vector a
row r m = V.slice start (matrixCols m) (matrixData m)
  where
    start = (r - 1) * matrixCols m

rows :: Matrix a -> Int
rows = matrixRows

shape :: Matrix a -> (Int, Int)
shape m = (matrixRows m, matrixCols m)

transpose :: Matrix a -> Matrix a
transpose m = Matrix (matrixCols m) (matrixRows m) transposedData
  where
    transposedData = V.generate (matrixRows m * matrixCols m) 
      (\i -> let r = i `div` matrixRows m
                 c = i `mod` matrixRows m
             in matrixData m V.! (c * matrixCols m + r))