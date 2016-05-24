module Data.TTask.Analysis 
  ( DailyStatuses(..)
  , dailyGroup
  , summaryPointBy 
  ) where
import Data.List
import Data.Maybe
import Data.Time
import Data.TTask.Types

data DailyStatuses = DailyStatuses
  { dayStDay :: Day
  , dayStPoint :: Point
  , dayStStatuses :: [StatusLogRec]
  }

dailyGroup :: [StatusLogRec] -> [DailyStatuses]
dailyGroup = catMaybes . map sr2ds . dailyGrouping 
  where
    sr2ds :: [StatusLogRec] -> Maybe DailyStatuses
    sr2ds srs = do
      (d, p) <- calcDaylyPoint srs
      return $ DailyStatuses
        { dayStDay = d
        , dayStPoint = p
        , dayStStatuses = srs
        }

----

calcDaylyPoint :: [StatusLogRec] -> Maybe (Day, Point)
calcDaylyPoint [] = Nothing
calcDaylyPoint xs@(x:_) = Just . (,) (getDay x) $ summaryPointBy isFinishedTask xs
  where
    isFinishedTask :: StatusLogRec -> Bool
    isFinishedTask r 
      = (stFinished $ stRecToStatus r) && (isTask $ stRecToContents r)

dailyGrouping :: [StatusLogRec] -> [[StatusLogRec]]
dailyGrouping = groupBy (\l r -> getDay l == getDay r) . sortRecord
  where
    sortRecord :: [StatusLogRec] -> [StatusLogRec]
    sortRecord = sortBy (\l r -> getTime l `compare` getTime r)

----

summaryPointBy :: (StatusLogRec -> Bool) -> [StatusLogRec] -> Point
summaryPointBy f = foldl (+) 0 . map getPoint . filter f 

getDay :: StatusLogRec -> Day
getDay = localDay . getTime

getTime :: StatusLogRec -> LocalTime
getTime = getStatusTime . stRecToStatus

getPoint :: StatusLogRec -> Point
getPoint r = case stRecToContents r of
  TTaskProject v -> calcProjectPoint v
  TTaskSprint v -> calcSprintPoint v
  TTaskStory v -> calcStoryPoint v
  TTaskTask v -> _taskPoint v 


