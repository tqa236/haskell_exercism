module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Dummy

bearing :: Robot -> Bearing
bearing robot = North

coordinates :: Robot -> (Integer, Integer)
coordinates robot = (0, 0)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = error "You need to implement this function."

move :: Robot -> String -> Robot
move robot instructions = error "You need to implement this function."
