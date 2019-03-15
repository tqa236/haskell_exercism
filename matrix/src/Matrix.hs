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

import           Data.Vector (Vector, (!))

newtype Matrix a = Vector (Vector a) deriving (Eq, Show)

cols :: Matrix a -> Int
cols matrix = error "You need to implement this function."

column :: Int -> Matrix a -> Vector a
column x matrix = error "You need to implement this function."

flatten :: Matrix a -> Vector a
flatten matrix = error "You need to implement this function."

fromList :: [[a]] -> Matrix a
fromList xss = error "You need to implement this function."

fromString :: Read a => String -> Matrix a
fromString xs = Matrix $ map (map read . words) allLines
    where allLines = lines xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape dimensions matrix = error "You need to implement this function."

row :: Int -> Matrix a -> Vector a
row x matrix = matrix ! x

rows :: Matrix a -> Int
rows matrix = error "You need to implement this function."

shape :: Matrix a -> (Int, Int)
shape matrix = error "You need to implement this function."

transpose :: Matrix a -> Matrix a
transpose matrix = error "You need to implement this function."
