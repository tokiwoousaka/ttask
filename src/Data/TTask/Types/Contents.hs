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
