module Queens (boardString, canAttack) where

import           Data.Maybe

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = concatMap toBoard (take 128 [1,2..])
    where whitePosition = fromJust white
          toBoard i
            | i == 16 * fst whitePosition + 2 * snd whitePosition = "W"
            | i `mod` 16 == 0 = "\n"
            | even i = " "
            | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = error "You need to implement this function"
