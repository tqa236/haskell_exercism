module Acronym (abbreviate) where

import           Data.Char

abbreviate :: String -> String
abbreviate xs = filter (isUpper) xs
