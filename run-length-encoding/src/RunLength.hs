module RunLength (decode, encode) where

decode :: String -> String
decode x = x
decode (x:y:xs) = x
    where number = read x :: Int
decode encodedText = encodedText

encode :: String -> String
encode text = text
