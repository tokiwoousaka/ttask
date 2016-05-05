module Data.TTask.Command.Update
  ( updateTaskStatus 
  , updateStoryStatus 
  , updateSprintStatus 
  , updateTask 
  , updateStory 
  , updateSprint 
  )  where
import Data.TTask.Types

------
-- Update status

updateTaskStatus :: Id -> TStatusRecord -> Project -> Project
updateTaskStatus i r pj  
  = updateTask i (\t -> t { taskStatus = r `TStatusCons` taskStatus t }) pj

updateStoryStatus :: Id -> TStatusRecord -> Project -> Project
updateStoryStatus i r pj  
  = updateStory i (\s -> s { storyStatus = r `TStatusCons` storyStatus s }) pj

updateSprintStatus :: Id -> TStatusRecord -> Project -> Project
updateSprintStatus i r pj  
  = updateSprint i (\s -> s { sprintStatus = r `TStatusCons` sprintStatus s }) pj

------
-- Update contents

updateTask :: Id -> (Task -> Task) -> Project -> Project
updateTask i f pj = pj
    { projectBacklog = map (updateStorysTask i f) $ projectBacklog pj
    , projectSprints = map (updateSprintsTask i f) $ projectSprints pj
    }

updateStory :: Id -> (UserStory -> UserStory) -> Project -> Project
updateStory i f pj = pj
    { projectBacklog = map (updateStorysStory i f) $ projectBacklog pj
    , projectSprints = map (updateSprintsStory i f) $ projectSprints pj
    }

updateSprint :: Id -> (Sprint -> Sprint) -> Project -> Project
updateSprint i f pj 
  = pj { projectSprints = map (updateSprintsSprint i f) $ projectSprints pj } 

----

updateSprintsTask :: Id -> (Task -> Task) -> Sprint -> Sprint
updateSprintsTask i f sp 
  = sp { sprintStorys = map (updateStorysTask i f) $ sprintStorys sp}

updateStorysTask :: Id -> (Task -> Task) -> UserStory -> UserStory
updateStorysTask i f story 
  = story { storyTasks = map (updateTasksTask i f) $ storyTasks story } 

updateSprintsStory :: Id -> (UserStory -> UserStory) -> Sprint -> Sprint
updateSprintsStory i f sp 
  = sp { sprintStorys = map (updateStorysStory i f) $ sprintStorys sp }

updateStorysStory :: Id -> (UserStory -> UserStory) -> UserStory -> UserStory
updateStorysStory i f story = if i == storyId story then f story else story

updateSprintsSprint :: Id -> (Sprint -> Sprint) -> Sprint -> Sprint
updateSprintsSprint i f sp = if i == sprintId sp then f sp else sp

updateTasksTask :: Id -> (Task -> Task) -> Task -> Task
updateTasksTask i f task = if i == taskId task then f task else task

