module Data.TTask.File.Compatibility 
  ( resolution 
  ) where
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.TTask.Types.Types 
import Data.TTask.File.Compatibility.V0_1_0_0

type TryConvert a = EitherT Project IO a

resolution' :: String -> TryConvert ()
resolution' s = do
  liftIO $ putStrLn "That is not latest ttask project file."
  tryRead "0.1.0.0" s
  liftIO $ putStrLn "... convert failure"

tryRead :: String -> String -> TryConvert ()
tryRead v s = tryFromMaybe (successMsg v) $ readProject s

--------

resolution :: String -> IO (Maybe Project)
resolution s = do
  e <- runEitherT $ resolution' s
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
  [ "Success convert from old ttask project file (V = less than ", s, ")" ]
