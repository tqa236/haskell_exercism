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
             deriving (Eq, Show, Enum)

data Robot = Robot Bearing (Integer, Integer) deriving (Show)

bearing :: Robot -> Bearing
bearing (Robot b _)  = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ p) = p

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot [] = robot
move robot (x:xs) = case x of
    'L' -> move (turnLeft robot) xs
    'R' -> move (turnRight robot) xs
    _   -> move (advance robot) xs

advance :: Robot -> Robot
advance (Robot North (x, y)) = Robot North (x, y + 1)
advance (Robot East  (x, y)) = Robot East  (x + 1, y)
advance (Robot South (x, y)) = Robot South (x, y - 1)
advance (Robot West  (x, y)) = Robot West  (x - 1, y)

turnLeft :: Robot -> Robot
turnLeft (Robot b p) = Robot b' p
    where b' = toEnum $ (fromEnum b - 1) `mod` 4

turnRight :: Robot -> Robot
turnRight (Robot b p) = Robot b' p
    where b' = toEnum $ (fromEnum b + 1) `mod` 4
