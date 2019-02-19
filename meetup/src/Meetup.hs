module Meetup (Weekday(..), Schedule(..), meetupDay) where

import           Data.Time.Calendar      (Day, fromGregorian)
import           Data.Time.Calendar.Week (dayOfWeek)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month
    | dayOfWeek (fromGregorian year month 13) == weekday = fromGregorian year month 13
    | otherwise = fromGregorian year month 19
