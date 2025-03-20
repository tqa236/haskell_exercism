module Palindromes (largestPalindrome, smallestPalindrome) where

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

products :: Integer -> Integer -> [(Integer, (Integer, Integer))]
products minFactor maxFactor =
  [ (x * y, (x, y))
  | x <- [minFactor..maxFactor]
  , y <- [x..maxFactor]
  , isPalindrome (x * y)
  ]

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  if null palindromes
  then Nothing
  else Just (maximum values, findFactors maxValue)
  where
    palindromes = products minFactor maxFactor
    values = map fst palindromes
    maxValue = maximum values
    findFactors val = [factorPair | (value, factorPair) <- palindromes, value == val]

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  if null palindromes
  then Nothing
  else Just (minimum values, findFactors minValue)
  where
    palindromes = products minFactor maxFactor
    values = map fst palindromes
    minValue = minimum values
    findFactors val = [factorPair | (value, factorPair) <- palindromes, value == val]
