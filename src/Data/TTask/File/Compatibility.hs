module Data.TTask.File.Compatibility 
  ( resolution 
  ) where
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.TTask.Types.Types 
import qualified Data.TTask.File.Compatibility.V0_0_1_0 as V0_0_1_0

resolution' :: String -> TryConvert ()
resolution' s = do
  liftIO $ putStrLn "That is not latest ttask project file."
  tryRead "0.0.1.0" s $ V0_0_1_0.readProject
  liftIO $ putStrLn "... convert failure"

tryRead :: String -> String -> (String -> Maybe Project) -> TryConvert ()
tryRead v s f = tryFromMaybe (successMsg v) $ f s

--------

type TryConvert a = EitherT Project IO a

runConvert :: TryConvert a -> IO (Either Project a)
runConvert = runEitherT

--------

resolution :: String -> IO (Maybe Project)
resolution s = do
  e <- runConvert $ resolution' s
  case e of
    Right () -> return Nothing
    Left pj -> return $ Just pj
  
tryFromMaybe :: String -> Maybe Project -> TryConvert ()
tryFromMaybe msg (Just x) = do
  liftIO $ putStrLn msg
  left x
tryFromMaybe _ Nothing = return ()

successMsg :: String -> String
successMsg s = concat 
  [ "Success convert from old ttask project file (less than ", s, ")" ]
