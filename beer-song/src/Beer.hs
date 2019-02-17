module Beer (song) where

song :: String
song = concatMap verse (reverse [0..99])

verse :: Int -> String
verse 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
\Go to the store and buy some more, 99 bottles of beer on the wall.\n"
verse n = bottle n ++ " of beer on the wall, " ++ bottle n ++ " of beer.\n"
            ++ action n ++ bottle (n - 1) ++ " of beer on the wall.\n\n"
    where bottle x
            | x > 1 = show x ++ " bottles"
            | x == 1 = show x ++ " bottle"
            | otherwise = "no more bottles"
          action x
            | x > 1 = "Take one down and pass it around, "
            | otherwise = "Take it down and pass it around, "
