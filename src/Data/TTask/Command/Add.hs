module Data.TTask.Command.Add 
  ( newProject 
  , addNewStoryToPbl
  , addNewStoryToSprints
  , addNewSprint
  , addNewTask
  , addSprintToProject 
  , addStoryToPbl 
  , addStoryToPblFirst 
  , addStoryToPjSprints 
  , addTaskToProject 
  , getLocalTime 
  , projectsTaskMaxId 
  , projectStoryMaxId 
  , projectSprintMaxId 
  ) where
import Control.Lens (over, (^.))
import Data.Time
import Data.List.Extra
import Data.Maybe
import Data.TTask.Types
import Safe

------
-- Create new Contents

newProject :: String -> IO Project
newProject name = do
  lt <- getLocalTime
  return $ Project
    { _projectName = name
    , _projectBacklog = []
    , _projectSprints = []
    , _projectStatus = TStatusOne $ StatusWait lt
    }

newStory :: String -> Int -> IO UserStory
newStory description id = do
  lt <- getLocalTime
  return $ UserStory
    { _storyId = id
    , _storyDescription = description
    , _storyTasks = []
    , _storyStatus = TStatusOne $ StatusWait lt
    }

newTask :: String -> Point -> Id -> IO Task
newTask description point id = do
  lt <- getLocalTime
  return $ Task
    { _taskId = id
    , _taskDescription = description
    , _taskPoint = point
    , _taskStatus = TStatusOne $ StatusWait lt
    , _taskWorkTimes = []
    }

newSprint :: String -> Id -> IO Sprint
newSprint description id = do
  lt <- getLocalTime
  return $ Sprint
    { _sprintId = id
    , _sprintDescription = description
    , _sprintStorys = []
    , _sprintStatus = TStatusOne $ StatusWait lt
    }

------
-- Add new contents to project

addNewStoryToPbl :: String -> Project -> IO Project
addNewStoryToPbl description pj = do
  let nid = projectStoryMaxId pj + 1
  us <- newStory description nid
  return . over projectBacklog (flip snoc us) $ pj

addNewStoryToSprints :: Id -> String -> Project -> IO Project
addNewStoryToSprints spid description pj = do
    let nid = projectStoryMaxId pj + 1
    us <- newStory description nid
    return $ addStoryToPjSprints spid us pj

addNewSprint :: String -> Project -> IO Project
addNewSprint description pj = do
  let nid = projectSprintMaxId pj + 1
  sp <- newSprint description nid
  return $ addSprintToProject sp pj

addNewTask :: Point -> Id -> String -> Project -> IO Project
addNewTask point usid description pj = do
    let nid = projectsTaskMaxId pj + 1
    task <- newTask description point nid
    return $ addTaskToProject usid task pj

------
-- Add existing contents 

addSprintToProject :: Sprint -> Project -> Project
addSprintToProject sp pj  = pj { _projectSprints = snoc (_projectSprints pj) sp }

addStoryToPbl :: UserStory -> Project -> Project
addStoryToPbl us pj = pj { _projectBacklog = snoc (_projectBacklog pj) us }

addStoryToPblFirst :: UserStory -> Project -> Project
addStoryToPblFirst us pj = pj { _projectBacklog = us : _projectBacklog pj }

addStoryToPjSprints :: Id -> UserStory -> Project -> Project
addStoryToPjSprints i us pj = pj { _projectSprints = map (addStoryToSprint i us) $ _projectSprints pj }

addTaskToProject :: Id -> Task -> Project -> Project
addTaskToProject i task pj = pj
      { _projectBacklog = addTaskToStorys i task $ _projectBacklog pj
      , _projectSprints = addTaskToSprintList i task $ _projectSprints pj
      }

----

addStoryToSprint :: Id -> UserStory -> Sprint -> Sprint
addStoryToSprint id us sp = if _sprintId sp == id then
   sp { _sprintStorys = snoc (_sprintStorys sp) us } else sp

addTaskToSprintList :: Id -> Task -> [Sprint] -> [Sprint]
addTaskToSprintList id task = map $ addTaskToSprint id task

addTaskToSprint :: Id -> Task -> Sprint -> Sprint
addTaskToSprint id task sp = 
  sp { _sprintStorys = addTaskToStorys id task $ _sprintStorys sp  }

addTaskToStorys :: Id -> Task -> [UserStory] -> [UserStory]
addTaskToStorys id task = map $ addTaskToStory id task

addTaskToStory :: Id -> Task -> UserStory -> UserStory
addTaskToStory id task us = if _storyId us == id
  then us { _storyTasks = snoc (_storyTasks us) task } else us

------
-- Id Control

projectsTaskMaxIdMay :: Project -> Maybe Id
projectsTaskMaxIdMay = maximumMay . map _taskId . (^.allTasks)

projectsTaskMaxId :: Project -> Id
projectsTaskMaxId = fromMaybe 0 . projectsTaskMaxIdMay

projectStoryMaxIdMay :: Project -> Maybe Id
projectStoryMaxIdMay = maximumMay . map _storyId . (^.allStory)

projectStoryMaxId :: Project -> Id
projectStoryMaxId = fromMaybe 0 . projectStoryMaxIdMay

projectSprintMaxIdMay :: Project -> Maybe Id
projectSprintMaxIdMay = maximumMay . map _sprintId . _projectSprints

projectSprintMaxId :: Project -> Id
projectSprintMaxId = fromMaybe 0 . projectSprintMaxIdMay

---- 
-- Util 

getLocalTime :: IO LocalTime
getLocalTime = getZonedTime >>= return . zonedTimeToLocalTime

