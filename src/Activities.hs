{-# language LambdaCase #-}

module Activities where

----------------------- Imports

import Data
import ProjectS
import Project

import Data.List (nub)
import Data.Time
import Data.Time.Clock

----------------------- Activities Creation

activitiesFromProjects
  :: [(Name, Project)] -> [Activity]
activitiesFromProjects
   = putConstraintsIfEqualInterval
     . putConstraints
     . putIds
     . concat
     . map activitiesFromProject

activitiesFromProject
  :: (Name, Project) -> [Activity]
activitiesFromProject
  (projName, Project (CycleP bien cyc) (DurationP dur) res)
  = go (mayShift dur cyc) dur 0
  where
    mayShift
      :: DursH
      -> [(Int, OkInterval)]
      -> [(Int, OkInterval)]
    mayShift d ints
      | minimalIntervalDuration <= maximumDuration * 3 = shift ints
      | otherwise = ints
      where
        shift = \case
          (n, okInt1) : (m, okInt2) : xs
            -> (n, okInt2) : shift ((m, okInt2) : xs)
          x -> x

        minimalIntervalDuration
          = case bien of
              B  -> intervalDuration ints
              I  -> intervalDuration ints - maximumDuration
              En -> intervalDuration ints - maximumDuration

        intervalDuration = okIntDur . snd . head
        okIntDur = \case
          [] -> error "wat??"
          (True, FromTo fr to) : xs ->
            round $ realToFrac $
            diffUTCTime (zonedTimeToUTC to) (zonedTimeToUTC fr) / 60
          _ : xs -> okIntDur xs

        maximumDuration = maximum $ map takeDur d
        takeDur  = sum . map (durToMinutes . second) . third

        second (_,s,_) = s
        third  (_,_,t) = t

    go :: [(Int, OkInterval)] -> DursH -> Int -> [Activity]
    go _ [] _ = []
    go (c:cs) ds i =
      let (acts,ds') = makeActivities i c ds
      in acts ++ go cs ds' (i+1)

    makeActivities
      :: Int
      -> (Int, OkInterval)
      -> DursH
      -> ([Activity], DursH)
    makeActivities i (times, ints) = \case
      []   -> error "?????"
      durs -> ( makeActivityFromDurations i ints times durs
              , drop times durs )

    makeActivityFromDurations
      :: Int -> OkInterval -> Int -> DursH -> [Activity]
    makeActivityFromDurations _ _ 0  _ = []
    makeActivityFromDurations _ _ _ [] = [] -- ???
    makeActivityFromDurations i ints n (d:ds) = 
      makeActivityDivision i ints d ds
      : makeActivityFromDurations (i+1) ints (n-1) ds

    makeActivityDivision
      :: Int
      -> OkInterval
      -> DurH
      -> DursH
      -> Activity
    makeActivityDivision i ints (mStepName, mMess, x) restDurs
      = goAct x
      where
        goAct = \case
          [] -> error "WTF"
          (mActivityName, actDur, between) : divRest
            -> Activity
               (mergeNames projName mStepName mActivityName)
               NoIdYet
               (durToMinutes actDur)
               (accordingToBien i actDur ints)
               (getPlace res)
               Nothing
               (getConstr res)
               (getCost res)
               (fromMaybeToString mMess)
               (case divRest of
                   [] -> Nothing
                   _  -> Just (between, goAct divRest))

    fromMaybeToString :: Maybe String -> String
    fromMaybeToString m = case m of
      Just  x -> x
      Nothing -> ""

    mergeNames
      :: String -> Maybe String -> Maybe String -> String
    mergeNames proj mStep mAct = case (mStep,mAct) of
      (Nothing, Nothing) -> proj
      (Just st, Nothing) -> proj ++ " - " ++ st
      (Nothing, Just ac) -> proj ++ " - " ++ ac
      (Just st, Just ac) -> proj ++ " - " ++ st ++ " - " ++ ac

    bienCycle :: [(Int, OkInterval)] -> [(Int,OkInterval)]
    bienCycle = undefined

    accordingToBien :: Int -> Dur -> OkInterval -> OkInterval    
    accordingToBien i d x = case bien of
      B  -> x
      I  -> if i == 0 then iCase0  x else iCase  x
      En -> if i == 0 then enCase0 x else enCase x
      where

        iCase0 :: OkInterval -> OkInterval
        iCase0 = \case
          [] -> []
          [(True, intT)]
            -> mergeInt (True, minusTo d intT)
          (True, intT) : (False, intF) : xs
            -> mergeInt (True, minusTo d intT)
               ++ iCase ((False, minusFrom d intF) : xs)
          c : cs -> c : iCase cs

        iCase :: OkInterval -> OkInterval
        iCase = firstCase . iCase0

        firstCase = \case
          (True, int) : xs
            -> mergeInt (True, minusTo d (minusFrom d int)) ++ xs
          y -> y

        enCase' (v, FromTo fr to)
          = (v, FromTo (minusDur d fr) (minusDur d to))

        enCase :: OkInterval -> OkInterval
        enCase = map enCase'

        enCase0 :: OkInterval -> OkInterval
        enCase0 = mapNotFirst enCase'
          where
            mapNotFirst f [] = []
            mapNotFirst f ((v, int) : rest)
              = mergeInt (v, minusTo d int) ++ map f rest

        minusTo
          :: Dur
          -> Interval ZonedTime
          -> Maybe (Interval ZonedTime)
        minusTo dur (FromTo fr to)
          | minusDur dur to < fr = Nothing
          | otherwise = Just $ FromTo fr (minusDur dur to)

        minusFrom :: Dur -> Interval ZonedTime -> Interval ZonedTime
        minusFrom dur (FromTo fr to) = FromTo (minusDur dur fr) to

        minusDur :: Dur -> ZonedTime -> ZonedTime
        minusDur dur = addTime (-durToMinutes dur)

        mergeInts
          :: InOrOut (Maybe (Interval ZonedTime))
          -> InOrOut (Interval ZonedTime)
          -> [InOrOut (Interval ZonedTime)]
        mergeInts (v, Nothing) (n, int) = [(n, int)]
        mergeInts (v, Just  i) (n, int) = [(v, i), (n, int)]

        mergeInt
          :: InOrOut (Maybe (Interval ZonedTime))
          -> [InOrOut (Interval ZonedTime)]
        mergeInt (v, Nothing) = []
        mergeInt (v, Just  i) = [(v, i)]

putIds :: [Activity] -> [Activity]
putIds = fst . go 0
  where
    go :: Int -> [Activity] -> ([Activity], Int)
    go n [] = ([], n)
    go n (act:xs) = case getMaybeDiv act of
      Nothing -> let act' = act {getId = Id n}
                     (acts',m) = go (n+1) xs
                 in (act' : acts', m)
      Just (int, next) ->
        let act' = act {getId = Id n}
            (acts , m) = go (n+1) [next]
            (acts', k) = go m xs
        in (act' : (acts ++ acts'), k)

putConstraints :: [Activity] -> [Activity]
putConstraints = map putCs
  where
    putCs :: Activity -> Activity
    putCs act = case getMaybeDiv act of
      Nothing -> act
      Just (int, next)
        -> act {getMaybeDiv = Just (int, putAfter act next)}

putConstraintsIfEqualInterval ::  [Activity] -> [Activity]
putConstraintsIfEqualInterval = go where
  go  [] = []
  go [x] = [x]
  go (x:y:rest)
    | isSameProject x y &&
      isSameInterval x y = x : go (putAfter x y : rest)
    | otherwise = x : go (y : rest)
    where
      isSameProject x y =
        let upToHifen = init . takeWhile (/='-')
            nx = upToHifen $ getName x
            ny = upToHifen $ getName y
        in nx == ny
      isSameInterval x y =
        let ix = getOkInterval x
            iy = getOkInterval y
        in ix == iy

----------------------- Helpers

putAfter :: Activity -> Activity -> Activity
putAfter a b
  = let cons = getConstraints b
    in b {getConstraints = nub $ AfterID (getId a) : cons}
