{-# LANGUAGE OverloadedStrings #-}

module Brackets (arePaired) where

import           Data.Text (Text, isInfixOf, pack, replace)

arePaired :: String -> Bool
arePaired xs = replaceBracket $ pack $ filter (`elem` ("{[()]}" :: String)) xs

replaceBracket :: Text -> Bool
replaceBracket "" = True
replaceBracket xs
    | "[]" `isInfixOf` xs = replaceBracket (replace "[]" "" xs)
    | "()" `isInfixOf` xs = replaceBracket (replace "()" "" xs)
    | "{}" `isInfixOf` xs = replaceBracket (replace "{}" "" xs)
    | otherwise = False
