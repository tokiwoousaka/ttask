module Data.TTask.Types 
  ( Point(..)
  , Id(..)
  , WorkTime(..)   
  , TStatusRecord(..) 
  , TStatus(..) 
  , Task(..) 
  , UserStory(..) 
  , Sprint(..) 
  , Project(..) 
  , sprintAllTasks 
  , projectsAllTasks 
  , projectsAllStory 
  , projectsTaskMaxId 
  , projectStoryMaxId 
  , projectSprintMaxId 
  , calcStoryPoint 
  , calcSprintPoint 
  , calcProjectPoint 
  , getUserStoryById 
  , getTaskById 
  , getSprintById 
  , getLastStatus 
  , getStatusTime
  , statusToList
  , isWait 
  , isRunning 
  , isFinished 
  , isNotAchieved 
  , isRejected 
  ) where
import Data.Time
import Data.List
import Data.Maybe
import Safe

type Point = Int
type Id = Int

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

------
-- List up Task/Story

sprintAllTasks :: Sprint -> [Task]
sprintAllTasks = concatMap storyTasks . sprintStorys

projectsAllTasks :: Project -> [Task]
projectsAllTasks p = concat 
  [ concatMap sprintAllTasks $ projectSprints p 
  , concatMap storyTasks $ projectBacklog p 
  ]

projectsAllStory :: Project -> [UserStory]
projectsAllStory p = concat 
  [ concatMap sprintStorys $ projectSprints p , projectBacklog $ p ]

------
-- Id Control

projectsTaskMaxIdMay :: Project -> Maybe Id
projectsTaskMaxIdMay = maximumMay . map taskId . projectsAllTasks

projectsTaskMaxId :: Project -> Id
projectsTaskMaxId = fromMaybe 0 . projectsTaskMaxIdMay

projectStoryMaxIdMay :: Project -> Maybe Id
projectStoryMaxIdMay = maximumMay . map storyId . projectsAllStory

projectStoryMaxId :: Project -> Id
projectStoryMaxId = fromMaybe 0 . projectStoryMaxIdMay

projectSprintMaxIdMay :: Project -> Maybe Id
projectSprintMaxIdMay = maximumMay . map sprintId . projectSprints

projectSprintMaxId :: Project -> Id
projectSprintMaxId = fromMaybe 0 . projectSprintMaxIdMay

------
-- Calclate sum of point

calcStoryPoint :: UserStory -> Point
calcStoryPoint = summaryContents storyTasks taskPoint

calcSprintPoint :: Sprint -> Point
calcSprintPoint = summaryContents sprintStorys calcStoryPoint

calcProjectPoint :: Project -> Point
calcProjectPoint pj 
  = summaryContents projectSprints calcSprintPoint pj 
  + summaryContents projectBacklog calcStoryPoint pj

summaryContents :: (a -> [b]) -> (b -> Point) -> a -> Point
summaryContents f g = foldr (+) 0 . map g . f 

------
-- Get contents by id

getUserStoryById :: Project -> Id -> Maybe UserStory
getUserStoryById pj i = find ((==i).storyId) $ projectsAllStory pj

getTaskById :: Project -> Id -> Maybe Task
getTaskById pj i = find ((==i).taskId) $ projectsAllTasks pj

getSprintById :: Project -> Id -> Maybe Sprint
getSprintById pj i = find ((==i).sprintId) $ projectSprints pj 

------
-- For Status 

getLastStatus :: TStatus -> TStatusRecord
getLastStatus (TStatusOne x) = x
getLastStatus (TStatusCons x _) = x

getStatusTime :: TStatusRecord -> LocalTime
getStatusTime (StatusWait t) = t
getStatusTime (StatusRunning t) = t
getStatusTime (StatusFinished t) = t
getStatusTime (StatusNotAchieved t) = t
getStatusTime (StatusReject t) = t

----

statusToList :: TStatus -> [TStatusRecord]
statusToList (TStatusOne x) = [x]
statusToList (TStatusCons x xs) = x : statusToList xs

isWait :: TStatus -> Bool
isWait s = case getLastStatus s of
  StatusWait _ -> True
  _ -> False

isRunning :: TStatus -> Bool
isRunning s = case getLastStatus s of
  StatusRunning _ -> True
  _ -> False

isFinished :: TStatus -> Bool
isFinished s = case getLastStatus s of
  StatusFinished _ -> True
  _ -> False

isNotAchieved :: TStatus -> Bool
isNotAchieved s = case getLastStatus s of
  StatusNotAchieved _ -> True
  _ -> False

isRejected :: TStatus -> Bool
isRejected s = case getLastStatus s of
  StatusReject _ -> True
  _ -> False
