module Diamond (diamond) where

import           Data.Char

diamond :: Char -> Maybe [String]
diamond c = Just (halfDiamond ++ tail (reverse halfDiamond))
    where halfDiamond = map (`makeLine` c) ['A'..c]

makeLine :: Char -> Char -> String
makeLine letter middleLetter = sidePad ++ coreString ++ sidePad
    where sidePadLength = ord middleLetter - ord letter
          sidePad = replicate sidePadLength ' '
          coreString = makeCoreString letter

makeCoreString :: Char -> String
makeCoreString letter
    | letter == 'A' = "A"
    | otherwise = [letter] ++ centerPad ++ [letter]
    where centerPadLength = 2 * (ord letter - ord 'A') - 1
          centerPad = replicate centerPadLength ' '
