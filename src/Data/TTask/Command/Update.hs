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
  = updateTask i (\t -> t { _taskStatus = r `TStatusCons` _taskStatus t }) pj

updateStoryStatus :: Id -> TStatusRecord -> Project -> Project
updateStoryStatus i r pj  
  = updateStory i (\s -> s { _storyStatus = r `TStatusCons` _storyStatus s }) pj

updateSprintStatus :: Id -> TStatusRecord -> Project -> Project
updateSprintStatus i r pj  
  = updateSprint i (\s -> s { _sprintStatus = r `TStatusCons` _sprintStatus s }) pj

------
-- Update contents

updateTask :: Id -> (Task -> Task) -> Project -> Project
updateTask i f pj = pj
    { _projectBacklog = map (updateStorysTask i f) $ _projectBacklog pj
    , _projectSprints = map (updateSprintsTask i f) $ _projectSprints pj
    }

updateStory :: Id -> (UserStory -> UserStory) -> Project -> Project
updateStory i f pj = pj
    { _projectBacklog = map (updateStorysStory i f) $ _projectBacklog pj
    , _projectSprints = map (updateSprintsStory i f) $ _projectSprints pj
    }

updateSprint :: Id -> (Sprint -> Sprint) -> Project -> Project
updateSprint i f pj 
  = pj { _projectSprints = map (updateSprintsSprint i f) $ _projectSprints pj } 

----

updateSprintsTask :: Id -> (Task -> Task) -> Sprint -> Sprint
updateSprintsTask i f sp 
  = sp { _sprintStorys = map (updateStorysTask i f) $ _sprintStorys sp}

updateStorysTask :: Id -> (Task -> Task) -> UserStory -> UserStory
updateStorysTask i f story 
  = story { _storyTasks = map (updateTasksTask i f) $ _storyTasks story } 

updateSprintsStory :: Id -> (UserStory -> UserStory) -> Sprint -> Sprint
updateSprintsStory i f sp 
  = sp { _sprintStorys = map (updateStorysStory i f) $ _sprintStorys sp }

updateStorysStory :: Id -> (UserStory -> UserStory) -> UserStory -> UserStory
updateStorysStory i f story = if i == _storyId story then f story else story

updateSprintsSprint :: Id -> (Sprint -> Sprint) -> Sprint -> Sprint
updateSprintsSprint i f sp = if i == _sprintId sp then f sp else sp

updateTasksTask :: Id -> (Task -> Task) -> Task -> Task
updateTasksTask i f task = if i == _taskId task then f task else task

