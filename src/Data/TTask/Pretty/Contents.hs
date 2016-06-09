module Data.TTask.Pretty.Contents
  ( ppActive
  , ppStory 
  , ppStoryI 
  , ppStoryList 
  , ppSprint 
  , ppSprintList 
  , ppProjectPbl 
  , ppProjectSprintList 
  , ppProjectSprint 
  , ppProjectSprintDetail 
  , ppSprintHeaderDetail 
  , ppProjectStory 
  , ppProjectTask 
  , ppStatusRecord
  ) where
import Control.Applicative
import Control.Lens
import Data.TTask.Types
import Data.List

ppActive :: String -> Project -> String 
ppActive pid pj = let
    activeSprints :: Project -> [Sprint]
    activeSprints = filter ((^.isRunning) . _sprintStatus) . _projectSprints
  in concat
    [ ppProjectHeader pid pj 
    , if activeSprints pj /= [] then "\n\nActive sprint(s) :\n" else "\nRunning sprint is nothing"
    , intercalate "\n" . map ppSprintDetail $ activeSprints pj
    , if _projectBacklog pj /= [] then "\n\nProduct backlog :\n" else ""
    , ppProjectPbl pj
    ]

----

ppTask :: Task -> String
ppTask t = formatRecord "TASK" 
  (t^.taskId) (t^.taskPoint) 
  (t^.taskStatus) (t^.taskDescription)

ppStoryHeader :: UserStory -> String
ppStoryHeader s = formatRecord "STORY" 
  (s^.storyId) (s^.point) 
  (s^.storyStatus) (s^.storyDescription)

ppStory :: UserStory -> String
ppStory story = ppStoryI 1 story

ppStoryI :: Int -> UserStory -> String
ppStoryI r story = formatFamily r story ppStoryHeader _storyTasks ppTask

ppStoryList :: [UserStory] -> String
ppStoryList = intercalate "\n" . map ppStoryHeader

ppSprintHeader :: Sprint -> String
ppSprintHeader s = formatRecord "SPRINT" 
  (s^.sprintId) (s^.point) 
  (s^.sprintStatus) (s^.sprintDescription)

ppSprint :: Sprint -> String
ppSprint sprint = formatFamily 1 sprint ppSprintHeader _sprintStorys ppStoryHeader

ppSprintList :: [Sprint] -> String
ppSprintList = intercalate "\n" . map ppSprintHeader

ppProjectHeader :: String -> Project -> String
ppProjectHeader pid pj = 
  formatRecordShowedId "PROJECT" pid (pj^.point) (pj^.projectStatus) (pj^.projectName)

----

ppSprintDetail :: Sprint -> String
ppSprintDetail s 
    = formatFamily 1 s ppSprintHeaderDetail _sprintStorys $ \s -> ppStoryI 2 s

ppSprintHeaderDetail :: Sprint -> String
ppSprintHeaderDetail s = ppSprintHeader s ++ "\n" ++ ppStatus (_sprintStatus s)

----

ppProjectPbl :: Project -> String
ppProjectPbl = ppStoryList . _projectBacklog

ppProjectSprintList :: Project -> String
ppProjectSprintList = ppSprintList . _projectSprints

ppProjectSprint :: Id -> Project -> Maybe String 
ppProjectSprint i pj = ppSprint <$> pj^?sprint i

ppProjectSprintDetail :: Id -> Project -> Maybe String
ppProjectSprintDetail i pj = ppSprintDetail <$> pj^?sprint i

ppProjectStory :: Id -> Project -> Maybe String
ppProjectStory i pj = ppStory <$> pj^?story i

ppProjectTask :: Id -> Project -> Maybe String
ppProjectTask i pj = ppTask <$> pj^?task i

----

formatRecord :: String -> Id -> Point -> TStatus -> String -> String
formatRecord htype i point st description = 
  formatRecordShowedId htype (show i) point st description

formatRecordShowedId :: String -> String -> Point -> TStatus -> String -> String
formatRecordShowedId htype i point st description = concat
  [ htype ++ " - " , i , " : " , show $ point 
  , "pt [ " , ppStatusRecord $ st^.getLastStatus , " ] " , description
  ]

formatFamily :: Eq b => Int -> a -> (a -> String) -> (a -> [b]) -> (b -> String) -> String
formatFamily r x f g h = concat
  [ f x , if g x /= [] then "\n" else "" , intercalate "\n" . map (indent . h) $ g x ]
    where indent = ((concat $ replicate r "　　")++)

----

ppStatusRecord :: TStatusRecord -> String
ppStatusRecord (StatusWait _) = "Wait"
ppStatusRecord (StatusRunning _) = "Running"
ppStatusRecord (StatusFinished _) = "Finished"
ppStatusRecord (StatusNotAchieved _) = "Not Achieved"
ppStatusRecord (StatusReject _) = "Reject"

ppStatus :: TStatus -> String
ppStatus s = intercalate "\n" . map pps . reverse $ s^.statusToList
  where
    pps :: TStatusRecord -> String
    pps r = 
      let st = take 15 $ ppStatusRecord r ++ concat (replicate 16 " ")
      in "To " ++ st ++ " at " ++ show (r^.getStatusTime)
  
