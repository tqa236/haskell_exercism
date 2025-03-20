module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [concat [cell r c | c <- [0..7]] | r <- [0..7]]
  where
    cell r c
      | c == 7    = symbol r c
      | otherwise = symbol r c ++ " "

    symbol r c
      | Just (wr, wc) <- white, wr == r, wc == c = "W"
      | Just (br, bc) <- black, br == r, bc == c = "B"
      | otherwise = "_"


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) = sameRow || sameColumn || sameDiagonal
  where
    sameRow = r1 == r2
    sameColumn = c1 == c2
    sameDiagonal = abs (r1 - r2) == abs (c1 - c2)

