module Data.TTask.Types.Status
  ( getLastStatus 
  , getStatusTime
  , statusToList
  , stWait 
  , stRunning 
  , stFinished 
  , stNotAchieved 
  , stRejected 
  , isWait 
  , isRunning 
  , isFinished 
  , isNotAchieved 
  , isRejected 
  , stRecToContents 
  , stRecToStatus 
  , getTaskStatuses 
  , getStoryStatuses 
  , getSprintStatuses 
  , getTaskLastStatus 
  , getStoryLastStatuses 
  , getSprintLastStatuses 
  ) where
import Data.Time
import Data.TTask.Types.Types

getLastStatus :: TStatus -> TStatusRecord
getLastStatus (TStatusOne x) = x
getLastStatus (TStatusCons x _) = x

getStatusTime :: TStatusRecord -> LocalTime
getStatusTime (StatusWait t) = t
getStatusTime (StatusRunning t) = t
getStatusTime (StatusFinished t) = t
getStatusTime (StatusNotAchieved t) = t
getStatusTime (StatusReject t) = t

statusToList :: TStatus -> [TStatusRecord]
statusToList (TStatusOne x) = [x]
statusToList (TStatusCons x xs) = x : statusToList xs

----

stWait :: TStatusRecord -> Bool
stWait (StatusWait _) = True
stWait _ = False

stRunning :: TStatusRecord -> Bool
stRunning (StatusRunning _) = True
stRunning _ = False

stFinished :: TStatusRecord -> Bool
stFinished (StatusFinished _) = True
stFinished _ = False

stNotAchieved :: TStatusRecord -> Bool
stNotAchieved (StatusNotAchieved _) = True
stNotAchieved _ = False

stRejected :: TStatusRecord -> Bool
stRejected (StatusReject _) = True
stRejected _ = False

isWait :: TStatus -> Bool
isWait = stWait . getLastStatus 

isRunning :: TStatus -> Bool
isRunning = stRunning . getLastStatus

isFinished :: TStatus -> Bool
isFinished = stFinished . getLastStatus

isNotAchieved :: TStatus -> Bool
isNotAchieved = stNotAchieved . getLastStatus

isRejected :: TStatus -> Bool
isRejected = stRejected . getLastStatus

----

stRecToContents :: StatusLogRec -> TTaskContents
stRecToContents = fst

stRecToStatus :: StatusLogRec -> TStatusRecord
stRecToStatus = snd

----

getTaskStatuses :: Task -> [StatusLogRec]
getTaskStatuses t = map (\x -> (TTaskTask t, x)) . statusToList $ taskStatus t

getStoryStatuses :: UserStory -> [StatusLogRec]
getStoryStatuses u = 
  let uStatus = map (\x -> (TTaskStory u, x)) . statusToList $ storyStatus u
  in uStatus ++ concatMap getTaskStatuses (storyTasks u)
  
getSprintStatuses :: Sprint -> [StatusLogRec]
getSprintStatuses s = 
  let sStatus = map (\x -> (TTaskSprint s, x)) . statusToList $ sprintStatus s
  in sStatus ++ concatMap getStoryStatuses (sprintStorys s)

getTaskLastStatus :: Task -> StatusLogRec
getTaskLastStatus t = (\x -> (TTaskTask t, x)) . getLastStatus $ taskStatus t

getStoryLastStatuses :: UserStory -> [StatusLogRec]
getStoryLastStatuses u = 
  let uStatus = (\x -> (TTaskStory u, x)) . getLastStatus $ storyStatus u
  in uStatus : map getTaskLastStatus (storyTasks u)

getSprintLastStatuses :: Sprint -> [StatusLogRec]
getSprintLastStatuses s = 
  let sStatus = (\x -> (TTaskSprint s, x)) . getLastStatus $ sprintStatus s
  in sStatus : concatMap getStoryLastStatuses (sprintStorys s)
