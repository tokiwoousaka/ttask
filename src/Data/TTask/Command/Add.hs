module Data.TTask.Command.Add where
import Data.Time
import Data.List.Extra
import Data.TTask.Types

------
-- Create new Contents

newProject :: String -> IO Project
newProject name = do
  lt <- getLocalTime
  return $ Project
    { projectName = name
    , projectBacklog = []
    , projectSprints = []
    , projectStatus = TStatusOne $ StatusWait lt
    }

newStory :: String -> Int -> IO UserStory
newStory description id = do
  lt <- getLocalTime
  return $ UserStory
    { storyId = id
    , storyDescription = description
    , storyTasks = []
    , storyStatus = TStatusOne $ StatusWait lt
    }

newTask :: String -> Point -> Id -> IO Task
newTask description point id = do
  lt <- getLocalTime
  return $ Task
    { taskId = id
    , taskDescription = description
    , taskPoint = point
    , taskStatus = TStatusOne $ StatusWait lt
    , taskWorkTimes = []
    }

newSprint :: String -> Id -> IO Sprint
newSprint description id = do
  lt <- getLocalTime
  return $ Sprint
    { sprintId = id
    , sprintDescription = description
    , sprintStorys = []
    , sprintStatus = TStatusOne $ StatusWait lt
    }

------
-- Add new contents to project

addNewStoryToPbl :: String -> Project -> IO Project
addNewStoryToPbl description pj = do
  let nid = projectStoryMaxId pj + 1
  us <- newStory description nid
  return $ pj { projectBacklog = snoc (projectBacklog pj) us }

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
addSprintToProject sp pj  = pj { projectSprints = snoc (projectSprints pj) sp }

addStoryToPbl :: UserStory -> Project -> Project
addStoryToPbl us pj = pj { projectBacklog = snoc (projectBacklog pj) us }

addStoryToPblFirst :: UserStory -> Project -> Project
addStoryToPblFirst us pj = pj { projectBacklog = us : projectBacklog pj }

addStoryToPjSprints :: Id -> UserStory -> Project -> Project
addStoryToPjSprints i us pj = pj { projectSprints = map (addStoryToSprint i us) $ projectSprints pj }

addTaskToProject :: Id -> Task -> Project -> Project
addTaskToProject i task pj = pj
      { projectBacklog = addTaskToStorys i task $ projectBacklog pj
      , projectSprints = addTaskToSprintList i task $ projectSprints pj
      }

----

addStoryToSprint :: Id -> UserStory -> Sprint -> Sprint
addStoryToSprint id us sp = if sprintId sp == id then
   sp { sprintStorys = snoc (sprintStorys sp) us } else sp

addTaskToSprintList :: Id -> Task -> [Sprint] -> [Sprint]
addTaskToSprintList id task = map $ addTaskToSprint id task

addTaskToSprint :: Id -> Task -> Sprint -> Sprint
addTaskToSprint id task sp = 
  sp { sprintStorys = addTaskToStorys id task $ sprintStorys sp  }

addTaskToStorys :: Id -> Task -> [UserStory] -> [UserStory]
addTaskToStorys id task = map $ addTaskToStory id task

addTaskToStory :: Id -> Task -> UserStory -> UserStory
addTaskToStory id task us = if storyId us == id
  then us { storyTasks = snoc (storyTasks us) task } else us


---- 
-- Util 

getLocalTime :: IO LocalTime
getLocalTime = getZonedTime >>= return . zonedTimeToLocalTime

