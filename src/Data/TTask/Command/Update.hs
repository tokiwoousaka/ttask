module Data.TTask.Command.Update
  ( updateTaskStatus 
  , updateStoryStatus 
  , updateSprintStatus 
  )  where
import Control.Lens
import Data.TTask.Types

------
-- Update status

updateTaskStatus :: Id -> TStatusRecord -> Project -> Project
updateTaskStatus i r pj 
  = pj&task i%~ (\t -> t { _taskStatus = r `TStatusCons` _taskStatus t })

updateStoryStatus :: Id -> TStatusRecord -> Project -> Project
updateStoryStatus i r pj  
  = pj&story i%~ (\s -> s { _storyStatus = r `TStatusCons` _storyStatus s })

updateSprintStatus :: Id -> TStatusRecord -> Project -> Project
updateSprintStatus i r pj  
  = pj&sprint i%~ (\s -> s { _sprintStatus = r `TStatusCons` _sprintStatus s })

