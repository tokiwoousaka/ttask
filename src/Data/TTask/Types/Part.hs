module Data.TTask.Types.Part
  ( projectsAllStory 
  , stWait 
  , stRunning 
  , stFinished 
  , stNotAchieved 
  , stRejected 
  , getLastStatus' 
  , statusToList'
  ) where 
import Control.Lens
import Data.Maybe
import Data.TTask.Types.Types

projectsAllStory :: Project -> [UserStory]
projectsAllStory p = concat 
  [ concatMap _sprintStorys $ _projectSprints p , _projectBacklog $ p ]

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

getLastStatus' :: TStatus -> TStatusRecord
getLastStatus' (TStatusOne x) = x
getLastStatus' (TStatusCons x _) = x

statusToList' :: TStatus -> [TStatusRecord]
statusToList' (TStatusOne x) = [x]
statusToList' (TStatusCons x xs) = x : statusToList' xs
