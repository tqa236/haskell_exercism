module Meetup (Weekday(..), Schedule(..), meetupDay) where

import           Data.Time.Calendar          (Day, fromGregorian,
                                              gregorianMonthLength)
import           Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum, Bounded)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = fromGregorian year month (startDay + difference)
    where (_, _, dayOfWeek) = toWeekDate (fromGregorian year month startDay)
          difference = (fromEnum weekday + 1 - dayOfWeek) `mod` 7
          startDay = case schedule of
                        First  -> 1
                        Second -> 8
                        Third  -> 15
                        Fourth -> 22
                        Last   -> gregorianMonthLength year month - 6
                        Teenth -> 13
