module Bob (responseFor) where

import           Data.Char
import           Data.List

responseFor :: String -> String
responseFor xs
    | iSEmpty = "Fine. Be that way!"
    | isQuestion = checkYelling
    | isYelling = "Whoa, chill out!"
    | otherwise = "Whatever."
    where iSEmpty = all isSpace xs
          isYelling = all (not .isLower) xs && any isUpper xs
          isQuestion = "?" `isSuffixOf` filter (not . isSpace) xs
          checkYelling
            | isYelling = "Calm down, I know what I'm doing!"
            | otherwise = "Sure."
