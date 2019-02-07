module Bob (responseFor) where

import           Data.Char

responseFor :: String -> String
responseFor xs
    | iSEmpty = "Fine. Be that way!"
    | isQuestion = checkYelling
    | isYelling = "Whoa, chill out!"
    | otherwise = "Whatever."
    where iSEmpty = null (filter (not . isSpace) xs)
          isYelling = null (filter (isLower) xs) && not (null (filter (isUpper) xs))
          isQuestion = last ( filter (/= ' ') xs)  == '?'
          checkYelling
            | isYelling = "Calm down, I know what I'm doing!"
            | otherwise = "Sure."
