module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, modify)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set

-- Define the Robot type
data Robot = Robot { robotNameRef :: IORef String }

-- Define the RunState type
data RunState = RunState { usedNames :: Set String }

-- Initialize the state with no used names
initialState :: RunState
initialState = RunState Set.empty

-- Generate a random robot name
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
  RunState used <- get
  name <- liftIO generateName
  if name `Set.member` used
    then generateUniqueName
    else do
      modify $ \s -> s { usedNames = Set.insert name used }
      return name

-- Generate a random name
generateName :: IO String
generateName = do
  let letters = ['A'..'Z']
  let numbers = ['0'..'9']
  l1 <- randomRIO (0, length letters - 1)
  l2 <- randomRIO (0, length letters - 1)
  n1 <- randomRIO (0, length numbers - 1)
  n2 <- randomRIO (0, length numbers - 1)
  n3 <- randomRIO (0, length numbers - 1)
  return [letters !! l1, letters !! l2, numbers !! n1, numbers !! n2, numbers !! n3]

-- Create a new robot with a unique name
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- generateUniqueName
  ref <- liftIO $ newIORef name
  return $ Robot ref

-- Reset a robot's name to a new unique name
resetName :: Robot -> StateT RunState IO ()
resetName robot = do
  newName <- generateUniqueName
  liftIO $ writeIORef (robotNameRef robot) newName

-- Retrieve the name of a robot
robotName :: Robot -> IO String
robotName robot = readIORef (robotNameRef robot)
