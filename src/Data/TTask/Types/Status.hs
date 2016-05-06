module Data.TTask.Types.Status
  ( getLastStatus 
  , getStatusTime
  , statusToList
  , isWait 
  , isRunning 
  , isFinished 
  , isNotAchieved 
  , isRejected 
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
