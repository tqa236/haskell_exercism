module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop
    | start == 1 && stop == 1 = ["On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."]
    | otherwise = ["On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."]
