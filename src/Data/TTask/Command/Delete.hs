module Data.TTask.Command.Delete 
  ( deleteTask 
  , deleteStory 
  , deleteSprint 
  ) where
import Data.TTask.Types

deleteTask :: Id -> Project -> Project
deleteTask i pj = pj
  { _projectBacklog  = map (deleteTaskFromStory i) $ _projectBacklog pj 
  , _projectSprints = map (deleteTaskFromSprint i) $ _projectSprints pj 
  }

deleteStory :: Id -> Project -> Project
deleteStory i pj = pj
  { _projectBacklog  = filter (\u -> _storyId u /= i) $ _projectBacklog pj
  , _projectSprints = map (deleteStoryFromSprint i) $ _projectSprints pj 
  }

deleteSprint :: Id -> Project -> Project
deleteSprint i pj 
  = pj { _projectSprints = filter (\s -> _sprintId s /= i) $ _projectSprints pj }

----

deleteTaskFromStory :: Id -> UserStory -> UserStory
deleteTaskFromStory i s 
  = s { _storyTasks = filter (\t -> _taskId t /= i) $ _storyTasks s }

deleteTaskFromSprint :: Id -> Sprint -> Sprint
deleteTaskFromSprint i s 
  = s { _sprintStorys = map (deleteTaskFromStory i) $ _sprintStorys s }

deleteStoryFromSprint :: Id -> Sprint -> Sprint
deleteStoryFromSprint i s 
  = s { _sprintStorys = filter (\u -> _storyId u /= i) $ _sprintStorys s }


