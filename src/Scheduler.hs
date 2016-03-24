{-# language LambdaCase #-}

module Scheduler where

---------------------- Imports

import Data
import ProjectS
import Project
import Activities

import Data.Time

---------------------- Scheduler

-- slot duration
delta = 3 -- minutes
deltam = Minutes delta

stripPasts :: [Activity] -> IO ([Activity], ZonedTime)
stripPasts acts = do
  as <- mapM stripPast acts
  return (map fst as ,minimum $ map snd as)
  where
    stripPast :: Activity -> IO (Activity, ZonedTime)
    stripPast act = do
      now <- getZonedTime
      case getOkInterval act of
        [] -> do
          let init = nextMinuteSeed $ addDur deltam now
          return (act, init) 
        (v, FromTo fr to) : rs -> do
          let cutPoint = nextMinuteSeed $ addDur deltam now
              newFr | fr <= cutPoint = cutPoint
                    | otherwise      = fr
              newOkInt = (v, FromTo newFr to) : rs
          return (act { getOkInterval = newOkInt }, newFr)

limitSchedule :: Tree (Bool, Schedule) -> Tree (Bool, Schedule)
limitSchedule = fmap (\(b, sched) -> (b, take 10 sched))

scheduler :: ZonedTime -> [Activity] -> Tree (Bool, Schedule)
scheduler init acts =
  buildTree
  creation
  constraints
  successCase
  normalCase
  acts (Tree (initialSchedule init) [])
  where
    creation :: [Activity] -> Schedule -> [Schedule]
    creation (ac:_) sched =
      let okInt = getOkInterval ac
          dur = getMaxDuration ac
          okSched = mergeOkIntervalAndSchedule sched okInt
          numSlots = numOfSlots dur
      in allPossibilities ac numSlots okSched 

    numOfSlots :: MaxDuration -> Int
    numOfSlots n
      | rem delta n == 0 = div delta n
      | otherwise = 1 + div delta n

    hasSlots :: Int -> OkSchedule -> Bool
    hasSlots 0  _ = True 
    hasSlots n os = case os of
      (True , Slot actp _) : rest
        -> hasSlots (n-1) rest && actp == Nothing
      (False, Slot actp _) : rest -> False
      [] -> False
      
    allPossibilities
      :: Activity -> Int -> OkSchedule -> [Schedule]
    allPossibilities act ns = recF
      where
        recF = \case
          [] -> []
          okSched @ ((v, Slot actp int) : rest)
            | v == True &&
              actp == Nothing &&
              hasSlots ns okSched
                -> putInSchedule (makePrim act) ns okSched 
                   : recF rest
            | otherwise -> recF rest

    makePrim :: Activity -> ActivityPrim
    makePrim a
      = ActivityPrim
        (getName a)
        (getId a)
        (getMessage a)
        (getMaxDuration a)
        {-(takeActivityConstraints-} (getConstraints a) --)
        (getMaybePlace a)
        (getMaybeEndEvent a)

    {-takeActivityConstraints :: [Constraint] -> [ActivityConstraint]
    takeActivityConstraints = map toActCons
      where
        toActCons = \case
          After       name -> AfterAct name
          AfterID       id -> AfterActID id
          RightAfter  name -> RightAfterAct name-}

    putInSchedule :: ActivityPrim -> Int -> OkSchedule -> Schedule
    putInSchedule actPrim = recP
      where
        recP 0 oks = map takeBoolOut oks
        recP n ((True, Slot Nothing int) : rest)
          = Slot (Just actPrim) int : recP (n-1) rest
        recP _ _ = error "Something is very wrong."
        takeBoolOut (bool,x) = x
        
    mergeOkIntervalAndSchedule
      :: Schedule -> OkInterval -> OkSchedule
    mergeOkIntervalAndSchedule sched okInt
      = moksch okInt sched
      where
        moksch :: OkInterval -> Schedule -> OkSchedule
        moksch ((True, int) : rs) = changeSchedule int

        changeSchedule
          :: Interval ZonedTime -> Schedule -> OkSchedule
        changeSchedule int@(FromTo f t) = map changeSched
          where
            changeSched slot@(Slot a (FromTo f' t'))
              | (f' <= f  && f  < t') ||
                (f  <  f' && t' < t ) ||
                (f' <  t  && t <= t') = (True, slot)
              | otherwise = (False, slot)

    constraints :: Schedule -> Bool
    constraints sched = True --go sched
      where
        
        isThereActAfter :: Int -> Name -> Schedule -> Bool
        isThereActAfter id name = undefined
          {-undefined -- ????
          . filter ((==name) . actName)
          . takeWhile ((/=id) . actID)

        actName (ActivityPrim na _ _ _ _ _ _) = undefined
        actID   (ActivityPrim _ (Id id) _ _ _ _ _) = undefined-}
        
        isThereIDAfter :: Int -> Int -> Schedule -> Bool
        isThereIDAfter = undefined
        
        isThereActRightAfter :: Int -> Name -> Schedule -> Bool
        isThereActRightAfter = undefined
        
        go :: Schedule -> Bool
        go = \case
          [] -> True
          Slot Nothing _ : ss -> go ss
          Slot (Just (ActivityPrim _ (Id id) _ _ cs _ _)) _ : ss
            -> mapAnd check cs && go ss
            where
            
              mapAnd :: (a -> Bool) -> [a] -> Bool
              mapAnd f = and . map f

              check :: Constraint -> Bool
              check = \case
                After name -> isThereActAfter id name sched
                AfterID (Id idAf) -> isThereIDAfter id idAf sched
                RightAfter name ->
                  isThereActRightAfter id name sched
                  
    getConstrsFromAct (ActivityPrim _ _ _ _ cs _ _) = cs
    getNameFromAct    (ActivityPrim na _ _ _ _ _ _) = na
    
    successCase :: Schedule -> (Bool, Schedule)
    successCase x = (True, x)

    normalCase :: Schedule -> (Bool, Schedule)
    normalCase x = (False, x)

    initialSchedule = l . initialSchedule' where
      l = take 200
    initialSchedule' :: ZonedTime -> Schedule
    initialSchedule' initialTime
      = let to = addDur (Minutes delta) initialTime
        in Slot Nothing (FromTo initialTime to)
           : initialSchedule to

getSuccesses :: Tree (Bool, Schedule) -> [Schedule]
getSuccesses = map snd . getElementsFromTree fst

{-  where
    goSched :: Tree Schedule -> [Activity] -> Tree Schedule
    goSched (Tree sched []) = \case
      []     -> Tree sched
      a : as -> let newSched = putActInSched a sched
                in goSched newSched as
        where
          putActInSched :: Activity -> Schedule -> Schedule
          putActInSched act sched =
            let act' = ActivityPrim
                       (getName act)
                       (getId act)
                       (getMessage act)
                       (getMaxDuration act)
                       undefined
                       undefined
                whereToPut = undefined
            in [Slot
               act'
               whereToPut
               (getMaybePlace act)
               (getMaybeEndEvent act)]-}

---------------------- Generic Tree Functions

buildTree :: ([b] -> a -> [a]) -> (a -> Bool)
          -> (a -> c) -> (a -> c)
          -> [b] -> (Tree a) -> (Tree c)
buildTree
  treeCreation checkConstraints
  successCase normalCase = go
  where
    go [] (Tree x []) = Tree (successCase x) []
    go (el:els) (Tree x [])
      = Tree (normalCase x)
        $ map (go els)
        $ treefy
        $ filter checkConstraints
        $ treeCreation (el:els) x
    treefy = map (\x -> Tree x [])

getElementsFromTree :: (a -> Bool) -> Tree a -> [a]
getElementsFromTree pred (Tree x rest)
  | pred    x = x : concat (map (getElementsFromTree pred) rest)
  | otherwise = concat (map (getElementsFromTree pred) rest)

---------------------- Helpers

addDur :: Dur -> ZonedTime -> ZonedTime
addDur dur = addTime (durToMinutes dur)

leaves :: Tree a -> [a]
leaves = leaves' []
  where 
    leaves' :: [a] -> Tree a -> [a]
    leaves' ans (Tree x []) = x : ans
    leaves' ans (Tree x xs) = concat $ map (leaves' ans) xs
