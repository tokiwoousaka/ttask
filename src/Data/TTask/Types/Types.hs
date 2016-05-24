{-# LANGUAGE TemplateHaskell #-}
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
  -- Lens accessers
  --, taskId 
  --, taskDescription 
  --, taskPoint 
  --, taskStatus 
  --, taskWorkTimes 
  --, storyId 
  --, storyDescription 
  --, storyTasks 
  --, storyStatus 
  --, sprintId 
  --, sprintDescription 
  --, sprintStorys 
  --, sprintStatus 
  --, projectName 
  --, projectBacklog 
  --, projectSprints
  --, projectStatus
  ) where
import Data.Time
import Control.Lens

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
  { _taskId :: Id
  , _taskDescription :: String
  , _taskPoint :: Int
  , _taskStatus :: TStatus
  , _taskWorkTimes :: [WorkTime]
  } deriving (Show, Read, Eq)
data UserStory = UserStory 
  { _storyId :: Id
  , _storyDescription :: String
  , _storyTasks :: [Task]
  , _storyStatus :: TStatus
  } deriving (Show, Read, Eq)
data Sprint = Sprint 
  { _sprintId :: Id
  , _sprintDescription :: String
  , _sprintStorys :: [UserStory]
  , _sprintStatus :: TStatus
  } deriving (Show, Read, Eq)
data Project = Project
  { _projectName :: String
  , _projectBacklog :: [UserStory]
  , _projectSprints :: [Sprint]
  , _projectStatus :: TStatus
  } deriving (Show, Read, Eq)

data TTaskContents
  = TTaskProject Project
  | TTaskSprint Sprint
  | TTaskStory UserStory
  | TTaskTask Task

makeLenses ''Task
makeLenses ''UserStory
makeLenses ''Sprint
makeLenses ''Project

