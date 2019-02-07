module Bob (responseFor) where

import           Data.Char

responseFor :: String -> String
responseFor xs
    | iSEmpty = "Fine. Be that way!"
    | isQuestion = checkLetter
    | noLetter = "Whatever."
    | noLowercaseLetter = "Whoa, chill out!"
    | otherwise = "Whatever."
    where iSEmpty = null (filter (not . isSpace) xs)
          noLowercaseLetter = null (filter (isLower) xs)
          isQuestion = last ( filter (/= ' ') xs)  == '?'
          noLetter = null (filter (isAlpha) xs)
          checkLetter
            | noLetter = "Sure."
            | noLowercaseLetter = "Calm down, I know what I'm doing!"
            | otherwise = "Sure."
