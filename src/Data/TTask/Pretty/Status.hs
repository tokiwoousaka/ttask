module Data.TTask.Pretty.Status
  ( ppProjectSprintLog 
  ) where
import Control.Applicative
import Control.Lens
import Data.List
import Data.Time
import Data.TTask.Types
import Data.TTask.Analysis 
import Data.TTask.Pretty.Contents

ppProjectSprintLog :: Id -> Project -> Maybe String
ppProjectSprintLog i pj = ppDailySprintLog <$> pj^?sprint i

ppDailySprintLog :: Sprint -> String
ppDailySprintLog s = 
  let 
    sx = s^.lastStatuses
    cond f r = (r^.getLogStatus.f) && (r^.getLogContents.isTask)
    summary f = show (summaryPointBy (cond f) sx)
  in concat
      [ ppSprintHeaderDetail s
      , "\n\n" 
      , intercalate "\n" . map ppDailyStatuses . dailyGroup $ s^.statuses
      , "\n\n" 
      , "Wait : " ++ summary isWait ++ "pt\n"
      , "Running : " ++ summary isRunning ++ "pt\n"
      , "Finished : " ++ summary isFinished ++ "pt\n"
      , "Not Achieved : " ++ summary isNotAchieved ++ "pt\n"
      , "Rejected : " ++ summary isRejected ++ "pt"
      ]

----

ppDailyStatuses :: DailyStatuses -> String
ppDailyStatuses d = concat
  [ show (dayStDay d), " : Total Finished point = ", show (dayStPoint d), "\n"
  , intercalate "\n" . map (("    "++) . ppStatusLog) $ dayStStatuses d
  ]

ppStatusLog :: StatusLogRec -> String
ppStatusLog s = case s^.getLogContents of
  TTaskProject v -> 
    fmtStatusRec "PROJECT" 0 (v^.point) s (_projectName v)
  TTaskSprint  v -> 
    fmtStatusRec "SPRINT" (v^.sprintId) (v^.point) s (_sprintDescription v)
  TTaskStory   v -> 
    fmtStatusRec "STORY" (v^.storyId) (v^.point) s (_storyDescription v)
  TTaskTask    v -> 
    fmtStatusRec "TASK" (v^.taskId) (v^.point) s (_taskDescription v)

fmtStatusRec 
  :: String -> Id -> Point -> StatusLogRec -> String -> String
fmtStatusRec s i p r d = concat
  [ "[", s, " ", show p, "pt ", stAndLt, "] ", show i, " : ", d ]
    where
      stAndLt :: String
      stAndLt = concat 
        [ ppStatusRecord (r^.getLogStatus)
        , " at ", show . localTimeOfDay $ r^.getLogStatus.getStatusTime
        ]
