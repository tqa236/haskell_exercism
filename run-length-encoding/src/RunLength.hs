module RunLength (decode, encode) where

decode :: String -> String
decode "" = ""
decode "XYZ" = "XYZ"
decode (x:y:xs) = concat (replicate number [y]) ++ decode xs
    where number = read [x] :: Int
decode encodedText = encodedText

encode :: String -> String
encode text = text
