module Data.TTask.Types.Class
  ( HasPoint(..)
  , HasTask(..)
  , HasStatuses(..)
  , IsStatus(..)
  ) where
import Control.Lens
import Data.TTask.Types.Types
import Data.TTask.Types.Part

------------
-- Point

class HasPoint p where
  calcPoint :: p -> Point
instance HasPoint Task where
  calcPoint = _taskPoint
instance HasPoint UserStory where
  calcPoint = summaryContents _storyTasks _taskPoint
instance HasPoint Sprint where
  calcPoint = summaryContents _sprintStorys calcPoint
instance HasPoint Project where
  calcPoint pj
    = summaryContents _projectSprints calcPoint pj 
    + summaryContents _projectBacklog calcPoint pj
instance HasPoint TTaskContents where
  calcPoint (TTaskProject v) = calcPoint v
  calcPoint (TTaskSprint v) = calcPoint v
  calcPoint (TTaskStory v) = calcPoint v
  calcPoint (TTaskTask v) = calcPoint v

summaryContents :: (a -> [b]) -> (b -> Point) -> a -> Point
summaryContents f g = foldr (+) 0 . map g . f 

---------
-- Task

class HasTask t where
  getTask :: t -> [Task]
instance HasTask UserStory where
  getTask = _storyTasks
instance HasTask Sprint where
  getTask = concatMap _storyTasks . _sprintStorys
instance HasTask Project where
  getTask p = concat 
    [ concatMap getTask $ _projectSprints p 
    , concatMap _storyTasks $ _projectBacklog p 
    ]

---------
-- Statuses

class HasStatuses s where
  getStatuses :: s -> [StatusLogRec]
  getLastStatuses :: s -> [StatusLogRec]
instance HasStatuses Task where
  getStatuses t = map (\x -> (TTaskTask t, x)) . statusToList' $ _taskStatus t
  getLastStatuses t = (:[]) . (\x -> (TTaskTask t, x)) . getLastStatus' $ _taskStatus t
instance HasStatuses UserStory where
  getStatuses u = 
    let uStatus = map (\x -> (TTaskStory u, x)) . statusToList' $ _storyStatus u
    in uStatus ++ concatMap getStatuses (u^.storyTasks)
  getLastStatuses u = 
    let uStatus = (\x -> (TTaskStory u, x)) . getLastStatus' $ _storyStatus u
    in uStatus : concatMap getLastStatuses (u^.storyTasks)
instance HasStatuses Sprint where
  getStatuses s = 
    let sStatus = map (\x -> (TTaskSprint s, x)) . statusToList' $ _sprintStatus s
    in sStatus ++ concatMap getStatuses (s^.sprintStorys)
  getLastStatuses s =
    let sStatus = (\x -> (TTaskSprint s, x)) . getLastStatus' $ _sprintStatus s
    in sStatus : concatMap getLastStatuses (s^.sprintStorys)

class IsStatus s where
  status2Wait :: s -> Bool
  status2Running :: s -> Bool
  status2Finished :: s -> Bool
  status2NotAchieved :: s -> Bool
  status2Rejected :: s -> Bool
instance IsStatus TStatusRecord  where
  status2Wait = stWait
  status2Running = stRunning
  status2Finished = stFinished
  status2NotAchieved = stNotAchieved
  status2Rejected = stRejected
instance IsStatus TStatus where
  status2Wait = stWait . getLastStatus'
  status2Running = stRunning . getLastStatus'
  status2Finished = stFinished . getLastStatus'
  status2NotAchieved = stNotAchieved . getLastStatus'
  status2Rejected = stRejected . getLastStatus'
