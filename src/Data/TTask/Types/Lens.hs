{-# LANGUAGE RankNTypes #-} 
module Data.TTask.Types.Lens
  ( LensPrism
  , LensPrism'
  , task 
  , story 
  , sprint 
  , taskInSprint 
  , storyInSprint 
  , taskInStroy 
  , allTasks 
  , allStory
  , point 
  , statuses 
  , lastStatuses 
  , getLastStatus
  , statusToList
  , isWait 
  , isRunning 
  , isFinished 
  , isNotAchieved 
  , isRejected 
  , getStatusTime 
  , getLogContents 
  , getLogStatus 
  , isProject 
  , isSprint 
  , isStory 
  , isTask 
  -----
  , bundle
  , finding
  ) where
import Data.List
import Data.Maybe
import Data.Time
import Control.Lens
import Data.TTask.Types.Types
import Data.TTask.Types.Class
import Data.TTask.Types.Part

type LensPrism s t a b = forall f.(Functor f, Applicative f) => Optic (->) f s t a b
type LensPrism' s a = LensPrism s s a a

task :: Id -> LensPrism' Project Task
task i = 
  (projectSprints.finding (sprintHasThatTask i)._Just.taskInSprint i)
  `bundle`
  (projectBacklog.finding (storyHasThatTask i)._Just.taskInStroy i)

story :: Id -> LensPrism' Project UserStory
story i = 
  (projectSprints.finding (sprintHasThatStory i)._Just.storyInSprint i)
  `bundle`
  (projectBacklog.finding ( (==i)._storyId)._Just)

sprint :: Id -> LensPrism' Project Sprint
sprint i = projectSprints.finding ( (==i)._sprintId )._Just

-----

taskInSprint :: Id -> LensPrism' Sprint Task
taskInSprint i = sprintStorys.finding (storyHasThatTask i)._Just.taskInStroy i

storyInSprint :: Id -> LensPrism' Sprint UserStory
storyInSprint i = sprintStorys.finding ( (==i)._storyId )._Just

taskInStroy :: Id -> LensPrism' UserStory Task
taskInStroy i = storyTasks.finding ( (==i)._taskId )._Just

-----

allTasks :: HasTask t => Getter t [Task]
allTasks = to getTask 

allStory :: Getter Project [UserStory]
allStory = to projectsAllStory

point :: HasPoint p => Getter p Point
point = to calcPoint

statuses :: HasStatuses s => Getter s [StatusLogRec]
statuses = to getStatuses 

lastStatuses :: HasStatuses s => Getter s [StatusLogRec]
lastStatuses = to getLastStatuses 

getLastStatus :: Getter TStatus TStatusRecord
getLastStatus = to getLastStatus'

statusToList :: Getter TStatus [TStatusRecord]
statusToList = to statusToList'

-----

isWait :: IsStatus s => Getter s Bool
isWait = to $ status2Wait

isRunning :: IsStatus s => Getter s Bool
isRunning = to $ status2Running

isFinished :: IsStatus s => Getter s Bool
isFinished = to $ status2Finished

isNotAchieved :: IsStatus s => Getter s Bool
isNotAchieved = to $ status2NotAchieved

isRejected :: IsStatus s => Getter s Bool
isRejected = to $ status2Rejected

getStatusTime :: Getter TStatusRecord LocalTime
getStatusTime = to f
  where
    f (StatusWait t) = t
    f (StatusRunning t) = t
    f (StatusFinished t) = t
    f (StatusNotAchieved t) = t
    f (StatusReject t) = t

-----

getLogContents :: Lens' StatusLogRec TTaskContents
getLogContents = _1

getLogStatus :: Lens' StatusLogRec TStatusRecord
getLogStatus = _2

-----

isProject :: Getter TTaskContents Bool
isProject = to f
  where 
    f (TTaskProject _) = True
    f _ = False

isSprint :: Getter TTaskContents Bool
isSprint = to f
  where
    f (TTaskSprint _) = True
    f _ = False

isStory :: Getter TTaskContents Bool
isStory = to f
  where
    f (TTaskStory _) = True
    f _ = False

isTask :: Getter TTaskContents Bool
isTask = to f
  where
    f (TTaskTask _) = True
    f _ = False

------
-- util

bundle :: LensPrism' a b -> LensPrism' a b -> LensPrism' a b
bundle l r f s = if isJust $ s^?l then l f s else r f s

finding :: (a -> Bool) -> Lens' [a] (Maybe a)
finding f = lens (find f) $ setToList f
  where
    setToList :: (a -> Bool) -> [a] -> (Maybe a) -> [a]
    setToList _ xs Nothing = xs
    setToList _ [] (Just y) = []
    setToList f (x:xs) m@(Just y) =
      if f x then y : xs else x : setToList f xs m

------
-- part

sprintHasThatStory :: Id -> Sprint -> Bool
sprintHasThatStory i s = isJust $ s^?storyInSprint i

sprintHasThatTask :: Id -> Sprint -> Bool
sprintHasThatTask i s = isJust $ s^?taskInSprint i

storyHasThatTask :: Id -> UserStory -> Bool
storyHasThatTask i s = isJust $ s^?taskInStroy i 
