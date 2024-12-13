module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount text =
  let normalizeWord = map toLower 
      isWordChar c = isAlphaNum c || c == '\''
      stripPunctuation = dropWhile (not . isAlphaNum) . reverse . dropWhile (not . isAlphaNum) . reverse
      tokenize s = case dropWhile (not . isWordChar) s of
                    "" -> []
                    s' -> let (word, rest) = span isWordChar s'
                          in stripPunctuation word : tokenize rest
      sortedWords = sort $ map normalizeWord (tokenize text)
  in map (\ws -> (head ws, length ws)) (group sortedWords)
