module Data.TTask.Pretty.Status
  ( ppProjectSprintLog 
  ) where
import Control.Applicative
import Data.List
import Data.Time
import Data.TTask.Types
import Data.TTask.Analysis 
import Data.TTask.Pretty.Contents

ppProjectSprintLog :: Id -> Project -> Maybe String
ppProjectSprintLog i pj = ppDailySprintLog <$> getSprintById pj i

ppDailySprintLog :: Sprint -> String
ppDailySprintLog s = 
  let 
    sx = getSprintLastStatuses s
    cond f r = (f $ stRecToStatus r) && (isTask $ stRecToContents r)
    summary f = show (summaryPointBy (cond f) sx)
  in concat
      [ ppSprintHeaderDetail s
      , "\n\n" 
      , intercalate "\n" . map ppDailyStatuses . dailyGroup $ getSprintStatuses s
      , "\n\n" 
      , "Wait : " ++ summary stWait ++ "pt\n"
      , "Running : " ++ summary stRunning ++ "pt\n"
      , "Finished : " ++ summary stFinished ++ "pt\n"
      , "Not Achieved : " ++ summary stNotAchieved ++ "pt\n"
      , "Rejected : " ++ summary stRejected ++ "pt"
      ]

----

ppDailyStatuses :: DailyStatuses -> String
ppDailyStatuses d = concat
  [ show (dayStDay d), " : Total Finished point = ", show (dayStPoint d), "\n"
  , intercalate "\n" . map (("    "++) . ppStatusLog) $ dayStStatuses d
  ]

ppStatusLog :: StatusLogRec -> String
ppStatusLog s = case stRecToContents s of
  TTaskProject v -> 
    fmtStatusRec "PROJECT" 0 (calcProjectPoint v) s (_projectName v)
  TTaskSprint  v -> 
    fmtStatusRec "SPRINT" (_sprintId v) (calcSprintPoint v) s (_sprintDescription v)
  TTaskStory   v -> 
    fmtStatusRec "STORY" (_storyId v) (calcStoryPoint v) s (_storyDescription v)
  TTaskTask    v -> 
    fmtStatusRec "TASK" (_taskId v) (_taskPoint v) s (_taskDescription v)

fmtStatusRec 
  :: String -> Id -> Point -> StatusLogRec -> String -> String
fmtStatusRec s i p r d = concat
  [ "[", s, " ", show p, "pt ", stAndLt, "] ", show i, " : ", d ]
    where
      stAndLt :: String
      stAndLt = concat 
        [ ppStatusRecord (stRecToStatus r)
        , " at ", show . localTimeOfDay . getStatusTime $ stRecToStatus r
        ]
