module Data.TTask.Types.Contents
  ( isProject 
  , isSprint 
  , isStory 
  , isTask 
  , sprintAllTasks 
  , projectsAllTasks 
  , projectsAllStory 
  , calcStoryPoint 
  , calcSprintPoint 
  , calcProjectPoint 
  , getUserStoryById 
  , getTaskById 
  , getSprintById 
  ) where
import Data.TTask.Types.Types
import Data.List

------
-- Screening contents

isProject :: TTaskContents -> Bool
isProject (TTaskProject _) = True
isProject _ = False

isSprint :: TTaskContents -> Bool
isSprint (TTaskSprint _) = True
isSprint _ = False

isStory :: TTaskContents -> Bool
isStory (TTaskStory _) = True
isStory _ = False

isTask :: TTaskContents -> Bool
isTask (TTaskTask _) = True
isTask _ = False

------
-- List up Task/Story

sprintAllTasks :: Sprint -> [Task]
sprintAllTasks = concatMap _storyTasks . _sprintStorys

projectsAllTasks :: Project -> [Task]
projectsAllTasks p = concat 
  [ concatMap sprintAllTasks $ _projectSprints p 
  , concatMap _storyTasks $ _projectBacklog p 
  ]

projectsAllStory :: Project -> [UserStory]
projectsAllStory p = concat 
  [ concatMap _sprintStorys $ _projectSprints p , _projectBacklog $ p ]

------
-- Calclate sum of point

calcStoryPoint :: UserStory -> Point
calcStoryPoint = summaryContents _storyTasks _taskPoint

calcSprintPoint :: Sprint -> Point
calcSprintPoint = summaryContents _sprintStorys calcStoryPoint

calcProjectPoint :: Project -> Point
calcProjectPoint pj 
  = summaryContents _projectSprints calcSprintPoint pj 
  + summaryContents _projectBacklog calcStoryPoint pj

summaryContents :: (a -> [b]) -> (b -> Point) -> a -> Point
summaryContents f g = foldr (+) 0 . map g . f 

------
-- Get contents by id

getUserStoryById :: Project -> Id -> Maybe UserStory
getUserStoryById pj i = find ((==i)._storyId) $ projectsAllStory pj

getTaskById :: Project -> Id -> Maybe Task
getTaskById pj i = find ((==i)._taskId) $ projectsAllTasks pj

getSprintById :: Project -> Id -> Maybe Sprint
getSprintById pj i = find ((==i)._sprintId) $ _projectSprints pj 
