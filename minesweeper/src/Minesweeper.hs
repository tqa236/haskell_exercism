module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board =
  let height = length board
      width = if null board then 0 else length (head board)
  in [ [ annotateCell board x y | y <- [0 .. width - 1] ] | x <- [0 .. height - 1] ]

annotateCell :: [String] -> Int -> Int -> Char
annotateCell board x y
  | board !! x !! y == '*' = '*'
  | otherwise =
      let adjacentMines = countAdjacentMines board x y
      in if adjacentMines == 0 then ' ' else head (show adjacentMines)

countAdjacentMines :: [String] -> Int -> Int -> Int
countAdjacentMines board x y =
  length [ () | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0), isMine board (x + dx) (y + dy) ]

isMine :: [String] -> Int -> Int -> Bool
isMine board x y =
  x >= 0 && x < length board && y >= 0 && y < length (head board) && board !! x !! y == '*'