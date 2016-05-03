{-# LANGUAGE ScopedTypeVariables #-}
module Data.TTask.Command.Move where
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.TTask.Types
import Data.TTask.Command.Add
import Data.TTask.Command.Delete

------
-- move contents

moveStoryToPbl :: Id -> Project -> Maybe Project
moveStoryToPbl uid pj = do
  us <- getUserStoryById pj uid
  return . addStoryToPblFirst us . deleteStory uid $ pj

moveStoryToSprints :: Id -> Id -> Project -> Maybe Project
moveStoryToSprints uid sid pj = do
  us <- getUserStoryById pj uid
  _ <- getSprintById pj sid
  return . addStoryToPjSprints sid us . deleteStory uid $ pj

moveTask :: Id -> Id -> Project -> Maybe Project
moveTask tid uid pj = do
  task <- getTaskById pj tid
  _ <- getUserStoryById pj uid
  return . addTaskToProject uid task . deleteTask tid $ pj

------
-- swap contents

swapSprint :: Id -> Id -> Project -> Project
swapSprint fid tid pj = 
  let (f2t, t2f) = swapFuncs pj sprintId getSprintById fid tid
  in pj { projectSprints = swapBy f2t t2f $ projectSprints pj }

swapStory :: Id -> Id -> Project -> Project
swapStory fid tid pj = 
  let (f2t, t2f) = swapFuncs pj storyId getUserStoryById fid tid
  in pj
    { projectBacklog = swapBy f2t t2f $ projectBacklog pj
    , projectSprints = map (swapSprintsStory fid tid pj) $ projectSprints pj
    }

swapTask :: Id -> Id -> Project -> Project
swapTask fid tid pj = 
  let (f2t, t2f) = swapFuncs pj taskId getTaskById fid tid
  in pj
    { projectBacklog = map (swapStorysTask fid tid pj) $ projectBacklog pj
    , projectSprints = map (swapSprintsTask fid tid pj) $ projectSprints pj
    }

----

swapSprintsTask :: Id -> Id -> Project -> Sprint -> Sprint
swapSprintsTask fid tid pj sp = 
  let (f2t, t2f) = swapFuncs pj taskId getTaskById fid tid
  in sp { sprintStorys = map (swapStorysTask fid tid pj) $ sprintStorys sp }

swapStorysTask :: Id -> Id -> Project -> UserStory -> UserStory
swapStorysTask fid tid pj story = 
  let (f2t, t2f) = swapFuncs pj taskId getTaskById fid tid
  in story { storyTasks = swapBy f2t t2f $ storyTasks story } 

swapSprintsStory :: Id -> Id -> Project -> Sprint -> Sprint
swapSprintsStory fid tid pj sp = 
  let (f2t, t2f) = swapFuncs pj storyId getUserStoryById fid tid
  in sp { sprintStorys = swapBy f2t t2f $ sprintStorys sp }

swapFuncs :: Project -> (a -> Id) 
  -> (Project -> Id -> Maybe a) -> Id -> Id -> (a -> Maybe a, a -> Maybe a)
swapFuncs pj f g fid tid = 
  (\v -> guard (f v == fid) >> g pj tid, \v -> guard (f v == tid) >> g pj fid)

----

swapBy :: forall a. (a -> Maybe a) -> (a -> Maybe a) -> [a] -> [a]
swapBy f g = swp
  where
    swp :: [a] -> [a]
    swp [] = []
    swp (x:xs) = fromMaybe x (f x <|> g x) : swp xs

