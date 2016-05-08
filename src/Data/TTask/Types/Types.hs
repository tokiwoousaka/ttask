module Data.TTask.Types.Types
  ( Point(..)
  , Id(..)
  , StatusLogRec
  , WorkTime(..)   
  , TStatusRecord(..) 
  , TStatus(..) 
  , Task(..) 
  , UserStory(..) 
  , Sprint(..) 
  , Project(..) 
  , TTaskContents(..)
  ) where
import Data.Time

type Point = Int
type Id = Int
type StatusLogRec = (TTaskContents, TStatusRecord)

newtype WorkTime = WorkTime Double deriving (Show, Read, Eq)
data TStatusRecord 
  = StatusWait LocalTime 
  | StatusRunning LocalTime 
  | StatusFinished LocalTime 
  | StatusNotAchieved LocalTime 
  | StatusReject LocalTime 
  deriving (Show, Read, Eq)
data TStatus 
  = TStatusCons TStatusRecord TStatus 
  | TStatusOne TStatusRecord 
  deriving (Show, Read, Eq)

data Task = Task 
  { taskId :: Id
  , taskDescription :: String
  , taskPoint :: Int
  , taskStatus :: TStatus
  , taskWorkTimes :: [WorkTime]
  } deriving (Show, Read, Eq)
data UserStory = UserStory 
  { storyId :: Id
  , storyDescription :: String
  , storyTasks :: [Task]
  , storyStatus :: TStatus
  } deriving (Show, Read, Eq)
data Sprint = Sprint 
  { sprintId :: Id
  , sprintDescription :: String
  , sprintStorys :: [UserStory]
  , sprintStatus :: TStatus
  } deriving (Show, Read, Eq)
data Project = Project
  { projectName :: String
  , projectBacklog :: [UserStory]
  , projectSprints :: [Sprint]
  , projectStatus :: TStatus
  } deriving (Show, Read, Eq)

data TTaskContents
  = TTaskProject Project
  | TTaskSprint Sprint
  | TTaskStory UserStory
  | TTaskTask Task
