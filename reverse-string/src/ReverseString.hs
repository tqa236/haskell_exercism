module ReverseString (reverseString) where

reverseString :: String -> String
reverseString str = reverseHelper str []
  where
    reverseHelper [] acc     = acc
    reverseHelper (x:xs) acc = reverseHelper xs (x:acc)
