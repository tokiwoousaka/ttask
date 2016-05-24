module Data.TTask.File.File
  ( Success(..)
  , readActiveProject
  , writeActiveProject
  , activeProjectName
  , setActiveProject
  , initDirectory 
  , initProjectFile 
  , findProjects 
  ) where
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.TTask.Types
import Data.TTask.Command
import qualified Data.TTask.File.Compatibility as C
import Safe
import System.Directory
import System.FilePath

data Success = Success | Failure deriving (Show, Read, Eq)

writeProject :: String -> Project -> IO ()
writeProject fn pj = writeFile fn $ show pj

readProject :: String -> IO (Maybe Project)
readProject fn = do
  d <- readFile fn
  return $ readMay d

readActiveProject :: IO (Maybe Project)
readActiveProject = do
  dir <- projectsDirectory
  mfn <- activeProjectName 
  mpj <- join <$> sequence (readProject . (dir </>) <$> mfn)
  case mpj of
    Just pj -> return mpj
    Nothing -> do
      s <- sequence (readFile . (dir </>) <$> mfn)
      join <$> (s `seq` sequence (C.resolution <$> s))

writeActiveProject :: Project -> IO Success
writeActiveProject pj = do
  dir <- projectsDirectory
  mfn <- activeProjectName 
  res <- sequence $ writeProject <$> fmap (dir </>) mfn <*> pure pj
  case res of
    Just _ -> return Success
    Nothing -> return Failure

----

workDirectory :: IO String
workDirectory = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".ttask"

projectsDirectory :: IO String
projectsDirectory = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".ttask" </> "projects"

activeMemoryFile :: IO String
activeMemoryFile = do
  workDir <- workDirectory
  return $ workDir </> "active"

activeProjectName :: IO (Maybe String)
activeProjectName = do
  fn <- activeMemoryFile
  exist <- doesFileExist fn
  if exist
    then readFile fn >>= return . Just
    else do
      writeFile fn ""
      return Nothing
    
setActiveProject :: String -> IO Success
setActiveProject id = do
  fn <- activeMemoryFile
  files <- findProjects
  if elem id files
    then do
      writeFile fn id
      return Success
    else return Failure

----

initDirectory :: IO ()
initDirectory = do
  workDirectory >>= createDirectoryIfMissing False
  projectsDirectory >>= createDirectoryIfMissing False

initProjectFile :: String -> String -> IO ()
initProjectFile id name = do
  pj <- newProject name
  fn <- projectsDirectory >>= return . (</> id)
  writeProject fn $ pj
  _ <- setActiveProject id --ファイル作成直後なので成功している気持ち……
  return ()

findProjects :: IO [String]
findProjects = do 
  files <- getDirectoryContentsMay =<< projectsDirectory
  return . filter (\s -> s /= "." && s /= "..") $ fromMaybe [] files

getDirectoryContentsMay :: String -> IO (Maybe [String])
getDirectoryContentsMay path = 
  (return . Just =<< getDirectoryContents path) `catch` through
    where
      through :: SomeException -> IO (Maybe a)
      through _ = return Nothing

----
-- util



