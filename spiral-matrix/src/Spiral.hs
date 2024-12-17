module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size
    | size <= 0 = []
    | otherwise = generateSpiral size
  where
    generateSpiral n = fill 1 (replicate n (replicate n 0)) (0, 0) (n-1, n-1)

    fill :: Int -> [[Int]] -> (Int, Int) -> (Int, Int) -> [[Int]]
    fill count matrix (top, left) (bottom, right)
        | top > bottom || left > right = matrix
        | otherwise =
            let indices = spiralIndices (top, left) (bottom, right)
                updatedMatrix = foldl (\m (pos, val) -> setValue pos val m)
                                      matrix
                                      (zip indices [count .. count + length indices - 1])
            in fill (count + length indices) updatedMatrix (top + 1, left + 1) (bottom - 1, right - 1)

    spiralIndices :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    spiralIndices (top, left) (bottom, right) =
        [(top, col) | col <- [left .. right]] ++
        [(row, right) | row <- [top + 1 .. bottom]] ++
        [(bottom, col) | col <- reverse [left .. right - 1], bottom > top] ++
        [(row, left) | row <- reverse [top + 1 .. bottom - 1], left < right]

    setValue :: (Int, Int) -> Int -> [[Int]] -> [[Int]]
    setValue (row, col) val matrix =
        take row matrix ++
        [take col (matrix !! row) ++ [val] ++ drop (col + 1) (matrix !! row)] ++
        drop (row + 1) matrix
