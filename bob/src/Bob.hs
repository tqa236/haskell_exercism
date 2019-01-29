module Bob (responseFor) where

import           Data.Char


responseFor :: String -> String
responseFor xs = case (yell, question, no_alpha, empty) of
    (_, _, _,  True)    -> "Fine. Be that way!"
    (_, False, True, _) -> "Whatever."

    (False, True, _, _) -> "Sure."
    (_, True, True,  _) -> "Sure."
    (True, True, _, _)  -> "Calm down, I know what I'm doing!"
    (True, False, _, _) -> "Whoa, chill out!"
    _                   ->  "Whatever."
    where yell = filter (isAlpha) xs == filter (isAlpha) (map Data.Char.toUpper xs)
          question = last ( filter (/= ' ') xs)  == '?'
          no_alpha = xs == filter (not . isAlpha) xs
          empty = filter (not . isSpace) xs == []
