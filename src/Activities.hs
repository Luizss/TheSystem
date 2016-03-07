module Activities where

----------------------- Imports

import Data.Time
import Data
import ProjectS
----------------------- Activities Creation

activitiesFromProject
  :: (Name, Project) -> [Activity]
activitiesFromProject
  (projName, Project (CycleP bien cyc) (DurationP dur) res)
  = go cyc dur
  where
    go _ [] = []
    go (c:cs) (d:ds) = makeActivities c d ++ go cs ds

    makeActivities
      :: (Int, [InOrOut (Interval ZonedTime)])
      -> (Maybe Name, Maybe Message
         , [(Maybe Name, Dur, Interval Dur)])
      -> [Activity]
    makeActivities (times, ints) (mStepName, mMess, divs)
      = replicate times $ makeActivityDivision divs
      where
        makeActivityDivision
          :: [(Maybe Name, Dur, Interval Dur)]
          -> Activity
        makeActivityDivision g = case g of
          []     -> undefined
          (mActivityName, actDur, between) : divRest
            -> Activity
               (mergeNames projName mStepName mActivityName)
               (durToMinutes actDur)
               (accordingToBien actDur bien ints)
               (getPlace res)
               Nothing -- EndEvent
               (getConstr res)
               (getCost res)
               (fromMaybeToString mMess)
               (case divRest of
                   [] -> Nothing
                   _  -> Just (between
                              , makeActivityDivision divRest))

        fromMaybeToString :: Maybe String -> String
        fromMaybeToString m = case m of
          Just x  -> x
          Nothing -> ""

        mergeNames
          :: String -> Maybe String -> Maybe String -> String
        mergeNames proj mStep mAct = case (mStep,mAct) of
          (Nothing,Nothing) -> proj
          (Just st,Nothing) -> proj ++ " - " ++ st
          (Nothing,Just ac) -> proj ++ " - " ++ ac
          (Just st,Just ac) -> proj ++ " - " ++ st ++ " - " ++ ac

        accordingToBien
          :: Dur
          -> BIEn
          -> [InOrOut (Interval ZonedTime)]
          -> [InOrOut (Interval ZonedTime)]
        accordingToBien _ B  x = x
        accordingToBien _ I  x = undefined
        accordingToBien _ En x = undefined
