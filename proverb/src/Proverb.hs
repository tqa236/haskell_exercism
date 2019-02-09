module Proverb(recite) where

recite :: [String] -> String
recite []     = ""
recite [x]    = "And all for the want of a " ++ x ++ "."
recite (x:xs) = repeatPhrase (x : xs) ++ recite [x]

repeatPhrase :: [String] -> String
repeatPhrase (x:y:xs)
    | not (null xs) = repeatPhrase(x:[y]) ++ repeatPhrase (y : xs)
    | otherwise = "For want of a " ++ x ++ " the " ++ y ++ " was lost.\n"
