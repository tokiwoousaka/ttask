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
import Data.TTask.Types
import Data.List

ppActive :: String -> Project -> String 
ppActive pid pj = let
    activeSprints :: Project -> [Sprint]
    activeSprints = filter (isRunning . _sprintStatus) . _projectSprints
  in concat
    [ ppProjectHeader pid pj 
    , if activeSprints pj /= [] then "\n\nActive sprint(s) :\n" else "\nRunning sprint is nothing"
    , intercalate "\n" . map ppSprintDetail $ activeSprints pj
    , if _projectBacklog pj /= [] then "\n\nProduct backlog :\n" else ""
    , ppProjectPbl pj
    ]

----

ppTask :: Task -> String
ppTask task = formatRecord "TASK" 
  (_taskId task) (_taskPoint task) 
  (_taskStatus task) (_taskDescription task)

ppStoryHeader :: UserStory -> String
ppStoryHeader story = formatRecord "STORY" 
  (_storyId story) (calcStoryPoint story) 
  (_storyStatus story) (_storyDescription story)

ppStory :: UserStory -> String
ppStory story = ppStoryI 1 story

ppStoryI :: Int -> UserStory -> String
ppStoryI r story = formatFamily r story ppStoryHeader _storyTasks ppTask

ppStoryList :: [UserStory] -> String
ppStoryList = intercalate "\n" . map ppStoryHeader

ppSprintHeader :: Sprint -> String
ppSprintHeader sprint = formatRecord "SPRINT" 
  (_sprintId sprint) (calcSprintPoint sprint) 
  (_sprintStatus sprint) (_sprintDescription sprint)

ppSprint :: Sprint -> String
ppSprint sprint = formatFamily 1 sprint ppSprintHeader _sprintStorys ppStoryHeader

ppSprintList :: [Sprint] -> String
ppSprintList = intercalate "\n" . map ppSprintHeader

ppProjectHeader :: String -> Project -> String
ppProjectHeader pid pj = 
  formatRecordShowedId "PROJECT" pid (calcProjectPoint pj) (_projectStatus pj) (_projectName pj)

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
ppProjectSprint i pj = ppSprint <$> getSprintById pj i

ppProjectSprintDetail :: Id -> Project -> Maybe String
ppProjectSprintDetail i pj = ppSprintDetail <$> getSprintById pj i

ppProjectStory :: Id -> Project -> Maybe String
ppProjectStory i pj = ppStory <$> getUserStoryById pj i

ppProjectTask :: Id -> Project -> Maybe String
ppProjectTask i pj = ppTask <$> getTaskById pj i

----

formatRecord :: String -> Id -> Point -> TStatus -> String -> String
formatRecord htype i point st description = 
  formatRecordShowedId htype (show i) point st description

formatRecordShowedId :: String -> String -> Point -> TStatus -> String -> String
formatRecordShowedId htype i point st description = concat
  [ htype ++ " - " , i , " : " , show $ point 
  , "pt [ " , ppStatusRecord . getLastStatus $ st , " ] " , description
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
ppStatus s = intercalate "\n" . map pps . reverse $ statusToList s
  where
    pps :: TStatusRecord -> String
    pps r = 
      let st = take 15 $ ppStatusRecord r ++ concat (replicate 16 " ")
      in "To " ++ st ++ " at " ++ show (getStatusTime r)
  
