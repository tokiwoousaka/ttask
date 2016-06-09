module Data.TTask.File.Compatibility.V0_0_1_0 
  ( readProject
  ) where
import Data.Functor
import Safe
import Data.Time
import Control.Lens
import qualified Data.TTask.Types.Types as T

data Task = Task 
  { taskId :: T.Id
  , taskDescription :: String
  , taskPoint :: Int
  , taskStatus :: T.TStatus
  , taskWorkTimes :: [T.WorkTime]
  } deriving (Show, Read, Eq)
data UserStory = UserStory 
  { storyId :: T.Id
  , storyDescription :: String
  , storyTasks :: [Task]
  , storyStatus :: T.TStatus
  } deriving (Show, Read, Eq)
data Sprint = Sprint 
  { sprintId :: T.Id
  , sprintDescription :: String
  , sprintStorys :: [UserStory]
  , sprintStatus :: T.TStatus
  } deriving (Show, Read, Eq)
data Project = Project
  { projectName :: String
  , projectBacklog :: [UserStory]
  , projectSprints :: [Sprint]
  , projectStatus :: T.TStatus
  } deriving (Show, Read, Eq)

data TTaskContents
  = TTaskProject Project
  | TTaskSprint Sprint
  | TTaskStory UserStory
  | TTaskTask Task

-------

readProject :: String -> Maybe T.Project
readProject = (convert <$>) . readOldProject 

readOldProject :: String -> Maybe Project
readOldProject s = readMay s

convert :: Project -> T.Project
convert p = T.Project
  { T._projectName = projectName p
  , T._projectBacklog = map convertStory $ projectBacklog p
  , T._projectSprints = map convertSprint $ projectSprints p
  , T._projectStatus = projectStatus p
  } 

-------

convertTask :: Task -> T.Task
convertTask t = T.Task
  { T._taskId = taskId t
  , T._taskDescription = taskDescription t
  , T._taskPoint = taskPoint t
  , T._taskStatus = taskStatus t
  , T._taskWorkTimes = taskWorkTimes t
  }

convertStory :: UserStory -> T.UserStory
convertStory u = T.UserStory
  { T._storyId = storyId u
  , T._storyDescription = storyDescription u
  , T._storyTasks = map convertTask $ storyTasks u
  , T._storyStatus = storyStatus u
  } 

convertSprint :: Sprint -> T.Sprint
convertSprint s = T.Sprint
  { T._sprintId = sprintId s
  , T._sprintDescription = sprintDescription s
  , T._sprintStorys = map convertStory $ sprintStorys s
  , T._sprintStatus = sprintStatus s
  } 

