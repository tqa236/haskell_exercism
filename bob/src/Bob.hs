module Bob (responseFor) where

import           Data.Char


responseFor :: String -> String
responseFor xs = case (no_lower, question, no_upper, empty) of
    (_, _, _, True)     -> "Fine. Be that way!"
    (_, False, True, _) -> "Whatever."
    (_, _, True, _)     -> "Sure."
    (False, True, _, _) -> "Sure."
    (_, True, _, _)     -> "Calm down, I know what I'm doing!"
    (True, _, _, _)     -> "Whoa, chill out!"
    _                   ->  "Whatever."
    where no_lower = filter (isLower) xs == []
          question = last ( filter (/= ' ') xs)  == '?'
          no_upper = filter (isUpper) xs == []
          empty = filter (not . isSpace) xs == []
