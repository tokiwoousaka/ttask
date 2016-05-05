{-# LANGUAGE DataKinds #-}
module Main where
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time
import Data.TTask
import Options.Declarative

main :: IO ()
main = do
  initDirectory
  run_ $ Group "Task management tool for yourself is inspired by scrum."
    [ subCmd "project" cmdProject
    , subCmd "add" cmdAdd
    , subCmd "delete" cmdDelete
    , subCmd "move" cmdMove
    , subCmd "swap" cmdSwap
    , subCmd "active" cmdActive
    , subCmd "pbl" cmdShowPbl 
    , subCmd "sprint" cmdShowSprints
    , subCmd "story" cmdShowStorys
    , subCmd "run" cmdRun
    , subCmd "finish" cmdFinish
    , subCmd "wait" cmdWait
    , subCmd "nota" cmdNotAchieved 
    , subCmd "reject" cmdReject
    ]

----
-- project

cmdProject 
  :: Flag "i" '["ID"] "Project id" "Switch project id" (Def "" String)
  -> Cmd "Switch other project. Or list all projects when project id is abbraviated." ()
cmdProject i = liftIO $ do
  case get i of 
    "" -> listupAllProjects
    j -> do
      res <- setActiveProject j
      when (res == Failure) 
        . putStrLn $ failureMsgWithIdS "Set active project" "project" j

listupAllProjects :: IO ()
listupAllProjects = do
    mpn <- activeProjectName
    pjs <- findProjects
    case mpn of
      Just pn -> do
        when (notElem pn pjs) 
          $ putStrLn ("Warning : active project `" ++ pn ++ "` is not found. Try `ttask project -i [project name]`")
        listProjects pn pjs
      Nothing -> do
        putStrLn "Warning : Get active project failed. Try `ttask project -i [project name]`"
  where
    listProjects :: String -> [String] -> IO ()
    listProjects pn = mapM_ $ putStrLn . fmtRow pn

    fmtRow :: String -> String -> String
    fmtRow apn lpn = (if apn == lpn then (" ["++).(++"]") else ("  "++)) lpn

----
-- add

cmdAdd :: Group
cmdAdd = Group "Add contents to active project"
  [ subCmd "project" cmdAddProject
  , subCmd "sprint" cmdAddSprint
  , subCmd "story" cmdAddStory
  , subCmd "task" cmdAddTask
  ]

cmdAddProject
  :: Arg "ID" String 
  -> Flag "n" '["Name"] "Project name" "New projects explanatory name" String
  -> Cmd "Add new project and switch to it" ()
cmdAddProject j n = liftIO $ do
  initProjectFile (get j) (get n)

cmdAddSprint
  :: Arg "DESCRIPTION" String 
  -> Cmd "Add sprint to active project" ()
cmdAddSprint description = liftIO . modifyActivePjSimple
    (addNewSprint $ get description) $ failureMsg "Add sprint"

cmdAddStory 
  :: Arg "DESCRIPTION" String 
  -> Flag "i" '["ID"] "Sprint id" "Target Sprint Id" (Def "-1" Int) 
  -> Cmd "Add story to product backlog, Or designated sprint." ()
cmdAddStory description i = liftIO . modifyActiveProject
    (addStory (get i) $ get description) (failureMsg "Add story") 
      $ failureMsgWithId "Add story" "sprint" (get i)
  where
    addStory :: Id -> String -> Project -> IO Project
    addStory i description pj = case i of
      -1 -> addNewStoryToPbl description pj
      _  -> addNewStoryToSprints i description pj

cmdAddTask
  :: Arg "DESCRIPTION" String 
  -> Flag "i" '["ID"] "Story id" "Target Story Id" Int
  -> Flag "p" '["POINT"] "Estimated work point" "Estimated work point like 1, 2, 3, 5, 8 ..." Int
  -> Cmd "Add task to active project" ()
cmdAddTask description i point = liftIO . modifyActiveProject 
  (addNewTask (get point) (get i) (get description)) (failureMsg "Add task") 
    $ failureMsgWithId "Add task" "story" (get i)

----
-- delete

cmdDelete :: Group
cmdDelete = Group "Delete contents from active project"
  [ subCmd "sprint" cmdDeleteSprint
  , subCmd "story" cmdDeleteStory
  , subCmd "task" cmdDeleteTask
  ]

cmdDeleteSprint
  :: Flag "i" '["ID"] "Sprint id" "Target Sprint Id" Int
  -> Cmd "Delete sprint from active project" ()
cmdDeleteSprint i = liftIO . modifyActivePjSimple
  (return . deleteSprint (get i)) $ failureMsg "Delete sprint"

cmdDeleteStory
  :: Flag "i" '["ID"] "Story id" "Target Story Id" Int
  -> Cmd "Delete sprint from active project" ()
cmdDeleteStory i = liftIO . modifyActivePjSimple
  (return . deleteStory (get i)) $ failureMsg "Delete story"

cmdDeleteTask
  :: Flag "i" '["ID"] "Task id" "Target Task Id" Int
  -> Cmd "Delete sprint from active project" ()
cmdDeleteTask i = liftIO . modifyActivePjSimple
  (return . deleteTask (get i)) $ failureMsg "Delete task"

----
-- swap

cmdSwap :: Group
cmdSwap = Group "Swap contents of active project"
  [ subCmd "sprint" cmdSwapSprint
  , subCmd "story" cmdSwapStory
  , subCmd "task" cmdSwapTask
  ]

cmdSwapSprint 
  :: Flag "f" '["ID-FROM"] "Sprint id From" "Target Story Id : From" Int
  -> Flag "t" '["ID-TO"] "Sprint id To" "Target Story Id : To" Int
  -> Cmd "Swap sprint of active project" ()
cmdSwapSprint i j = liftIO . modifyActivePjSimple
  (return . swapSprint (get i) (get j)) $ failureMsg "Swap sprint"

cmdSwapStory 
  :: Flag "f" '["ID-FROM"] "Story id From" "Target Story Id : From" Int
  -> Flag "t" '["ID-TO"] "Story id To" "Target Story Id : To" Int
  -> Cmd "Swap story of active project" ()
cmdSwapStory i j = liftIO . modifyActivePjSimple
  (return . swapStory (get i) (get j)) $ failureMsg "Swap story"

cmdSwapTask 
  :: Flag "f" '["ID-FROM"] "Task id From" "Target Story Id : From" Int
  -> Flag "t" '["ID-TO"] "Task id To" "Target Story Id : To" Int
  -> Cmd "Swap task of active project" ()
cmdSwapTask i j = liftIO . modifyActivePjSimple
  (return . swapTask (get i) (get j)) $ failureMsg "Swap task"

----
-- move

cmdMove :: Group
cmdMove = Group "Move contents of active project"
  [ subCmd "story" cmdMoveStory
  , subCmd "task" cmdMoveTask
  ]

cmdMoveStory
  :: Flag "i" '["ID"] "Story id" "Target Story Id" Int
  -> Flag "t" '["SPRINT-ID"] "Sprint id" "Destination Srint Id" (Def "-1" Int)
  -> Cmd "Move story of active project" ()
cmdMoveStory i s = liftIO . modifyActivePjSimple
    (return . moveStory (get i) (get s)) $ failureMsg "Move story"
  where
    moveStory :: Id -> Id -> Project -> Project
    moveStory i s pj = fromMaybe pj $ case s of
      -1 -> moveStoryToPbl i pj
      _  -> moveStoryToSprints i s pj

cmdMoveTask 
  :: Flag "i" '["ID"] "Story id" "Target Story Id" Int
  -> Flag "t" '["STORY-ID"] "Story id" "Destination Story Id" Int
  -> Cmd "Move story of active project" ()
cmdMoveTask i s = liftIO . modifyActivePjSimple
  (\pj -> return . fromMaybe pj $ moveTask (get i) (get s) pj) $ failureMsg "Move task"

----
-- update

cmdRun :: Group
cmdRun = Group "Update contents status to `Running`"
  [ subCmd "sprint" (cmdUpdStatusSprint StatusRunning)
  , subCmd "story" (cmdUpdStatusStory StatusRunning)
  , subCmd "task" (cmdUpdStatusTask StatusRunning)
  ]

cmdFinish :: Group
cmdFinish = Group "Update contents status to `Finiesh`"
  [ subCmd "sprint" (cmdUpdStatusSprint StatusFinished)
  , subCmd "story" (cmdUpdStatusStory StatusFinished)
  , subCmd "task" (cmdUpdStatusTask StatusFinished)
  ]

cmdWait :: Group
cmdWait = Group "Update contents status to `Wait`"
  [ subCmd "sprint" (cmdUpdStatusSprint StatusWait)
  , subCmd "story" (cmdUpdStatusStory StatusWait)
  , subCmd "task" (cmdUpdStatusTask StatusWait)
  ]

cmdNotAchieved :: Group
cmdNotAchieved = Group "Update contents status to `Not Achieved`"
  [ subCmd "sprint" (cmdUpdStatusSprint StatusNotAchieved)
  , subCmd "story" (cmdUpdStatusStory StatusNotAchieved)
  , subCmd "task" (cmdUpdStatusTask StatusNotAchieved)
  ]

cmdReject :: Group
cmdReject = Group "Update contents status to `Rejected`"
  [ subCmd "sprint" (cmdUpdStatusSprint StatusReject)
  , subCmd "story" (cmdUpdStatusStory StatusReject)
  , subCmd "task" (cmdUpdStatusTask StatusReject)
  ]

cmdUpdStatusSprint :: (LocalTime -> TStatusRecord)
  -> Flag "i" '["ID"] "Sprint id" "Target Sprint Id" Int
  -> Cmd "Update sprint of active project" ()
cmdUpdStatusSprint s i = liftIO $ do
  lt <- getLocalTime
  modifyActivePjSimple
    (return . updateSprintStatus (get i) (s lt)) $ failureMsg "Update sprint"

cmdUpdStatusStory :: (LocalTime -> TStatusRecord)
  -> Flag "i" '["ID"] "Story id" "Target Story Id" Int
  -> Cmd "Update story of active project" ()
cmdUpdStatusStory s i = liftIO $ do
  lt <- getLocalTime
  modifyActivePjSimple
    (return . updateStoryStatus (get i) (s lt)) $ failureMsg "Update story"

cmdUpdStatusTask :: (LocalTime -> TStatusRecord)
  -> Flag "i" '["ID"] "Task id" "Target Task Id" Int
  -> Cmd "Update task of active project" ()
cmdUpdStatusTask s i = liftIO $ do
  lt <- getLocalTime
  modifyActivePjSimple
    (return . updateTaskStatus (get i) (s lt)) $ failureMsg "Update task"

----
-- show

cmdActive 
  :: Cmd "Show active projects information" ()
cmdActive = liftIO $ do
    pn <- activeProjectName
    case pn of
      Just n -> 
        execToActiveProject (putStrLn . ppActive n) $ failureMsg "Show project"
      Nothing -> putStrLn $ failureMsg "Show project"

cmdShowPbl 
  :: Cmd "List all pbl storys" ()
cmdShowPbl  = liftIO $ do
  execToActiveProject (putStrLn . ppProjectPbl) $ failureMsg "Show product backlog"

cmdShowSprints
  :: Flag "i" '["ID"] "Sprint id" "Target Sprint Id" (Def "-1" Int)
  -> Flag "s" '["SIMPLE"] "show simple" "Show simple mode when sprint id is designated." Bool
  -> Cmd "List all sprint, Or show sprints detail when project id is not abbraviated." ()
cmdShowSprints i s = liftIO $ do
    execToActiveProject (showSprint $ get i) $ failureMsg "Show sprint(s)"
  where
    showSprint :: Id -> Project -> IO ()
    showSprint i pj = 
      showContnt i pj ppProjectSprintList (if get s then ppProjectSprint else ppProjectSprintDetail)
      ("Error : Show sprint(s) Filure. Check target story (id = " ++ show i ++ ") is exist")

cmdShowStorys
  :: Flag "i" '["ID"] "Story id" "Target Story Id" Int
  -> Cmd "Show target story information and all tasks" ()
cmdShowStorys i = liftIO $ do
    execToActiveProject (showStory $ get i) $ failureMsg "Show story"
  where
    showStory :: Id -> Project -> IO ()
    showStory i pj = showContnt i pj (const "") ppProjectStory
      ("Error : Show story Filure. Check target story (id = " ++ show i ++ ") is exist")

----
-- util

modifyActivePjSimple :: (Project -> IO Project) -> String -> IO ()
modifyActivePjSimple f err = modifyActiveProject f err err

modifyActiveProject :: (Project -> IO Project) -> String -> String -> IO ()
modifyActiveProject f err1 err2 = do
  mpj <- readActiveProject
  case mpj of
    Just pj -> do
      res <- writeActiveProject =<< f pj
      when (res == Failure) 
        $ putStrLn err2
    Nothing -> putStrLn err1

execToActiveProject :: (Project -> IO ()) -> String -> IO ()
execToActiveProject f err = do
  mpj <- readActiveProject
  case mpj of
    Just pj -> f pj
    Nothing -> putStrLn err

showContnt :: Id -> Project -> (Project -> String) -> (Id -> Project -> Maybe String) -> String -> IO ()
showContnt i pj f g err = case i of
  -1 -> putStrLn (f pj)
  _  -> 
    let res = g i pj 
    in case res of 
      Just r -> putStrLn r
      Nothing -> putStrLn err

----

failureMsg :: String -> String
failureMsg s 
  = "Error : " ++ s ++ " Failure. Check active project is available as `ttask project`"

failureMsgWithId :: String -> String -> Id -> String
failureMsgWithId s t i = failureMsgWithIdS s t $ show i

failureMsgWithIdS :: String -> String -> String -> String
failureMsgWithIdS s t i 
  = "Error : " ++ s ++ " Failure. Check " ++ t ++ " `" ++ show i ++ "` exist"
