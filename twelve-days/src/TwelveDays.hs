module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop
    | start <= stop = reciteOneVerse start ++ recite (start + 1) stop
    | otherwise = []

reciteOneVerse :: Int -> [String]
reciteOneVerse stop = ["On the " ++ days ++ " day of Christmas my true love gave to me: " ++ gifts stop ++"."]
    where days = case stop of
            1  -> "first"
            2  -> "second"
            3  -> "third"
            4  -> "fourth"
            5  -> "fifth"
            6  -> "sixth"
            7  -> "seventh"
            8  -> "eighth"
            9  -> "ninth"
            10 -> "tenth"
            11 -> "eleventh"
            12 -> "twelfth"
            _  -> ""
gifts :: Int -> String
gifts stop
    | stop == 1 = gift
    | stop == 2 = gift ++ ", and " ++ gifts 1
    | otherwise = gift ++ ", " ++ gifts (stop - 1)
    where gift = case stop of
            1  -> "a Partridge in a Pear Tree"
            2  -> "two Turtle Doves"
            3  -> "three French Hens"
            4  -> "four Calling Birds"
            5  -> "five Gold Rings"
            6  -> "six Geese-a-Laying"
            7  -> "seven Swans-a-Swimming"
            8  -> "eight Maids-a-Milking"
            9  -> "nine Ladies Dancing"
            10 -> "ten Lords-a-Leaping"
            11 -> "eleven Pipers Piping"
            12 -> "twelve Drummers Drumming"
            _  -> ""
