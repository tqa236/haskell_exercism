module Diamond (diamond) where

import Data.Char (ord, isAlpha, toUpper)

diamond :: Char -> Maybe [String]
diamond c
  | not (isAlpha c) = Nothing
  | otherwise = Just (top ++ tail (reverse top))
  where
    cUpper = toUpper c
    top = map (`makeLine` cUpper) ['A'..cUpper]

makeLine :: Char -> Char -> String
makeLine letter middleLetter = sidePad ++ coreString ++ sidePad
  where
    sidePadLength = ord middleLetter - ord letter
    sidePad = replicate sidePadLength ' '
    coreString = makeCoreString letter

makeCoreString :: Char -> String
makeCoreString letter
  | letter == 'A' = "A"
  | otherwise = [letter] ++ centerPad ++ [letter]
  where
    centerPadLength = 2 * (ord letter - ord 'A') - 1
    centerPad = replicate centerPadLength ' '
