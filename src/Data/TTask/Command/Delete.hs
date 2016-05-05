module Data.TTask.Command.Delete 
  ( deleteTask 
  , deleteStory 
  , deleteSprint 
  ) where
import Data.TTask.Types

deleteTask :: Id -> Project -> Project
deleteTask i pj = pj
  { projectBacklog  = map (deleteTaskFromStory i) $ projectBacklog pj 
  , projectSprints = map (deleteTaskFromSprint i) $ projectSprints pj 
  }

deleteStory :: Id -> Project -> Project
deleteStory i pj = pj
  { projectBacklog  = filter (\u -> storyId u /= i) $ projectBacklog pj
  , projectSprints = map (deleteStoryFromSprint i) $ projectSprints pj 
  }

deleteSprint :: Id -> Project -> Project
deleteSprint i pj 
  = pj { projectSprints = filter (\s -> sprintId s /= i) $ projectSprints pj }

----

deleteTaskFromStory :: Id -> UserStory -> UserStory
deleteTaskFromStory i s 
  = s { storyTasks = filter (\t -> taskId t /= i) $ storyTasks s }

deleteTaskFromSprint :: Id -> Sprint -> Sprint
deleteTaskFromSprint i s 
  = s { sprintStorys = map (deleteTaskFromStory i) $ sprintStorys s }

deleteStoryFromSprint :: Id -> Sprint -> Sprint
deleteStoryFromSprint i s 
  = s { sprintStorys = filter (\u -> storyId u /= i) $ sprintStorys s }


