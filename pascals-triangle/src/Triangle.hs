module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x
    | x > 1 = lastRow ++ newRow
    | x == 1 = [[1]]
    | otherwise = []
    where lastRow = rows $ x - 1
          lastRowElement = last lastRow
          newRow = [zipWith (+) (lastRowElement ++ [0]) (0 : lastRowElement)]
