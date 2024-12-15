module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (chr, ord, isLower)
import Control.Monad (replicateM)

shiftChar :: Int -> Char -> Char
shiftChar n c
  | isLower c = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'
  | otherwise = c

unshiftChar :: Int -> Char -> Char
unshiftChar n c = shiftChar (-n) c

caesarEncode :: String -> String -> String
caesarEncode key text = zipWith encodeChar (cycle key) text
  where
    encodeChar k t
      | isLower t = shiftChar (ord k - ord 'a') t
      | otherwise = t

caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith decodeChar (cycle key) encodedText
  where
    decodeChar k t
      | isLower t = unshiftChar (ord k - ord 'a') t
      | otherwise = t

randomKey :: Int -> IO String
randomKey n = replicateM n (randomRIO ('a', 'z'))

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey 100
  let encodedText = caesarEncode key text
  return (key, encodedText)