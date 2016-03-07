module StateAndInfer where

-------------------- Imports

import System.Directory
import Data
import ProjectS

import Data.Time
import Data.Time.Clock
import Control.Monad

-------------------- Getting State

getState :: IO [(Name,State)]
getState = do
  let stateFile = ".state"
  doesIt <- doesFileExist stateFile
  case doesIt of
    True -> do
      conts <- readFile stateFile
      let states = read conts :: [(Name,State)]
      putStrLn "Got State."
      return states
    False -> do
      let state = [] :: [(Name,State)]
      writeFile stateFile (show state)
      putStrLn "State Initialized."
      return state

-------------------- Merging State with Projects

mergeStates
  :: [(Name, State)] -> [(Name, ProjectS)] -> [(Name, ProjectS)]
mergeStates     [] projs = check projs
mergeStates (s:ss) projs = mergeStates ss (mergeState s projs)

check :: [(Name, ProjectS)] -> [(Name, ProjectS)]
check [] = error "No projects to be made."
check s  = s

mergeState
  :: (Name, State) -> [(Name, ProjectS)] -> [(Name, ProjectS)]
mergeState (name, state) projects
  = handleProject $ filter (\(n,_) -> n == name) projects
  where
    handleProject  [] = errorNoProject
    handleProject  ps
      | length ps > 2 = errorMoreThanOneProject
      | otherwise     = substOrDelete
                        (\(n,_) -> n == name)
                        (merge (head ps) state)
                        projects
    
    errorNoProject
       = error $ "No project with name " ++ name ++ "."

    errorMoreThanOneProject
      = error $ "More than one project with name " ++ name ++ "."

merge :: (Name,ProjectS) -> State -> Maybe (Name,ProjectS)
merge (name, ProjectS c d e t r) (State n ts)
  | stepsDone n  d == Finished ||
    timeSpent ts t == Finished = Nothing
  | otherwise =
    let Unfinished d' = stepsDone n d
        Unfinished t' = timeSpent ts t
    in Just (name, ProjectS c d' e t' r)
  where 
    stepsDone
      :: Maybe StepsDone
      -> Maybe DurationS
      -> IsFinished (Maybe DurationS)
    stepsDone Nothing d = Unfinished d
    stepsDone _ Nothing = Unfinished Nothing
    stepsDone (Just (StepsDone 0)) jd = Unfinished jd
    stepsDone (Just (StepsDone n)) (Just (DurationInfS d)) =
      error $ show $ Unfinished $ Just $ DurationInfS $ rotate n d
    stepsDone (Just (StepsDone n)) (Just (DurationS d)) = case d of
      [] -> Finished
      (name,dds) : xs
        | length dds > n ->
          Unfinished $ Just $ DurationS $ (name,drop n dds) : xs
        | otherwise ->
          stepsDone'
          (StepsDone (n - length dds))
          (DurationS xs)

    stepsDone'
      :: StepsDone
      -> DurationS
      -> IsFinished (Maybe DurationS)
    stepsDone' (StepsDone 0) _ = Finished
    stepsDone' (StepsDone n) (DurationS d)
      = case d of
      [] -> Finished
      (name,dds) : xs
        | length dds > n ->
          Unfinished $ Just $ DurationS $ (name,drop n dds) : xs
        | otherwise -> stepsDone'
                       (StepsDone (n - length dds))
                       (DurationS xs)

    timeSpent
      :: Maybe TimeSpent
      -> Maybe Totaltime
      -> IsFinished (Maybe Totaltime)
    timeSpent Nothing t = Unfinished t
    timeSpent _ Nothing = Unfinished Nothing
    timeSpent (Just (TimeSpent timeS)) (Just (Totaltime tot))
      | tot - timeS <= 0 = Finished
      | otherwise = Unfinished $ Just $ Totaltime $ tot - timeS
    timeSpent
      (Just (TimeSpent timeS))
      (Just (EffectiveTotaltime tot))
      | tot - timeS <= 0 = Finished
      | otherwise        =
        Unfinished $ Just $ EffectiveTotaltime $ tot - timeS

substProject
  :: Maybe ProjectS
  -> Name
  -> [(Name,ProjectS)]
  -> [(Name, ProjectS)]
substProject _ _ [] = []
substProject Nothing name ((n,p):projs)
  | name == n = projs
  | otherwise = (n,p ) : substProject Nothing name projs
substProject (Just p') name ((n,p):projs)
  | name == n = (n,p') : projs
  | otherwise = (n,p ) : substProject (Just p') name projs

{-makeProject :: [(Name,ProjectS)] -> State -> ProjectS
makeProject proj state = undefined

mergeS :: [(Name,ProjectS)] -> State -> [(Name,ProjectS)]
mergeS projs may = case maybe of
  Nothing     -> map Just projs
  Just []     -> map Just projs
  Just states -> mergeS' projs states
  where
    mergeS' projs sts = mergeS' (insertState projs s) sts

    insertState proj (name,s) = map f proj
      where
        f (n,p) | n == name && changeProj s p == [] = Nothing
                | n == name = Just (n, changeProj s p)
                | otherwise = Just (n, p)
                                  
    changeProj (ProjectS c d e t r) state = case state of
      StepsDone n -> ProjectS c (stepsDone n d) e t r
      TimeSpent x -> ProjectS c d e (timeSpent x t) r
      Both    x n -> ProjectS c (stepsDone n d) e (timeSpent x t) r

    stepsDone n d = drop n d
    timeSpent x t = t - x-}

-------------------- Infering

----- Helper Types
type CycleStd = Double
type DurStd   = [Double]
type EndStd   = Double
type TotalStd = Double
type K        = Double

infers
  :: [(Name,ProjectS)]
  -> [ProjectType]
  -> IO [(Name, (CycleS,DurationS,Totaltime,RestS))]
infers = zipWithM $ \(n,p) pt -> do
  r <- infer pt p
  return (n,r)

infer
  :: ProjectType
  -> ProjectS
  -> IO (CycleS, DurationS, Totaltime,RestS)
infer pType (ProjectS c d e t r) = putRest <$> case pType of
  CD    -> do
    let d' = stdDur d
    return (frj c, frj d, inferTotaltime d')
  CD'   -> do
    return (frj c, frj d, Infinity)
  DE    -> do
    let d' = stdDur d
        k  = len d'
    e' <- stdEndpoint e
    return ( inferCycle k e'
           , frj d
           , inferTotaltime d')
  CE    -> do
    let c' = stdCycle c
    e' <- stdEndpoint e
    (d',t') <- estimateDT_CE c' e'
    return (frj c, d', fromTotalStd t')
  ET    -> do
    let t' = stdTotaltime t
    e' <- stdEndpoint e
    (c',d') <- estimateCD_ET e' t'
    return (fromCycleStd c', d', frj t)
  CT    -> do
    let c' = stdCycle c
        t' = stdTotaltime t
    (d',_e') <- estimateDE_CT c' t'
    return (frj c, d', frj t)
  CDE   -> do
    let c' = stdCycle c
        d' = stdDur d
        k  = len d'
    e' <- stdEndpoint e
    {-putStr "c': "
    print c'
    putStr "e': "
    print e'
    putStrLn "c' * e': "
    print $ c' * e'
    putStr "k: "
    print k-}
    assert (floor $ c' * e') (floor k) "CDE: ce = k"
    return (frj c, frj d, inferTotaltime d')
  CD'E  -> do
    let c' = stdCycle c
    e' <- stdEndpoint e
    let d' = truncDurCE d c' e'
        d''= stdDur (Just d')
    return (frj c, d', inferTotaltime d'')
  CDT   -> do
    let c' = stdCycle c
        d' = stdDur d
        t' = stdTotaltime t
    assert (sum d') t' "CDT: sum d = t"
    return (frj c, frj d, frj t)
  CD'T  -> do
    let c' = stdCycle c
        t' = stdTotaltime t
        d' = truncDurT d t'
    return (frj c, d', frj t)
  CET   -> do
    let c' = stdCycle c
    e' <- stdEndpoint e
    let t' = stdTotaltime t
    return (frj c, inferDur c' e' t', frj t)
  DET   -> do --
    let d' = stdDur d
        k  = len d'
    e' <- stdEndpoint e
    let t' = stdTotaltime t
    assert (sum d') t' "DET: sum d = t"
    return (inferCycle k e', frj d, frj t)
  D'ET  -> do
    let t' = stdTotaltime t
        d' = truncDurT d t'
        k  = len $ stdDur (Just d')
    e' <- stdEndpoint e
    return (inferCycle k e', d', frj t)
  CDET  -> do
    let c' = stdCycle c
        d' = stdDur d
        k  = len d'
        t' = stdTotaltime t
    e' <- stdEndpoint e
    assert (sum d') t' "CDET: sum d = t"
    assert (floor $ c' * e') (floor k) "CDET: ce = k"
    return (frj c, frj d, frj t)
  CD'ET -> do
    let c' = stdCycle c
        t' = stdTotaltime t
        d' = truncDurT d t'
        d''= stdDur (Just d')
        k  = len d''
    e' <- stdEndpoint e
    assert (floor $ c' * e') (floor k) "CD'ET: ce = k"
    return (frj c, d', frj t)
  where putRest (a,b,c) = (a,b,c,r)
        
----- To Standard Types

stdCycle :: Maybe CycleS -> CycleStd
stdCycle (Just (CycleS times dur))
  = fromIntegral times / fromIntegral mins
  where mins = durToMinutes dur

stdDur :: Maybe DurationS -> DurStd --[[Minutes]]
stdDur (Just d) = case d of
  DurationS    lst -> toStd lst
  DurationInfS lst -> toStd lst
  where
    toStd = map (fromIntegral
                 . sum
                 . map durToMinutes
                 . takeDurations)
    takeDurations (_,l) = map (\(_,d,_) -> d) l

stdEndpoint :: Maybe Endpoint -> IO EndStd
stdEndpoint (Just (Endpoint end)) = do
  now <- getCurrentTime
  let t = (diffUTCTime (zonedTimeToUTC end) now) / 60
  return $ realToFrac t

stdTotaltime :: Maybe Totaltime -> TotalStd
stdTotaltime (Just (Totaltime          m)) = fromIntegral m
stdTotaltime (Just (EffectiveTotaltime m)) = fromIntegral m
stdTotaltime _ = error "Error: stdTotaltime"

----- From Standard Types

fromTotalStd :: TotalStd -> Totaltime
fromTotalStd = Totaltime . floor

fromCycleStd :: CycleStd -> CycleS
fromCycleStd timesPerMinutes
  = CycleS 1
    $ Minutes
    $ floor
    $ 1 / timesPerMinutes

----- Infering

inferCycle :: K -> EndStd -> CycleS
inferCycle k e = fromCycleStd $ k / e

inferTotaltime :: DurStd -> Totaltime
inferTotaltime = fromTotalStd . sum

inferDur :: CycleStd -> EndStd -> TotalStd -> DurationS
inferDur c e t = alwaysDur $ floor $ t / (c * e)

----- Estimation (depois)

estimateDT_CE :: CycleStd -> EndStd -> IO (DurationS,TotalStd)
estimateDT_CE cycle minutesTillEnd = do
  let d = 1.5
  putStrLn
    $ "Inferred: Duration of Project = " ++ show d ++ " hours."
  return
    ( alwaysDur $ floor $ d * 60
    , cycle * minutesTillEnd * (d * 60))

estimateCD_ET = undefined

estimateDE_CT = undefined

----- Truncation

truncDurT :: Maybe DurationS -> TotalStd -> DurationS
truncDurT d@(Just (DurationInfS xs)) t =
  let k = length
          $ takeWhile (<= t)
          $ scanl (+) 0
          $ cycle
          $ stdDur d
  in DurationS $ take k $ cycle xs

truncDurCE :: Maybe DurationS -> CycleStd -> EndStd -> DurationS
truncDurCE (Just (DurationInfS xs)) c e =
  let k = floor $ c * e
  in DurationS $ take k $ cycle xs

--- Truncar apos a funçao infer
truncate :: (CycleS, DurationS, Totaltime, RestS) -> IO DurationS
truncate (c, dur, t, _) = case (dur, t) of
  (DurationS    d,                     _) -> return $ DurationS d
  (DurationInfS d, Totaltime           m) -> return $ trunc d m
  (DurationInfS d, EffectiveTotaltime em) -> return $ trunc d em
  (DurationInfS d,              Infinity) -> truncMaximum d c
  where
    trunc d m =
      let k = length
              $ takeWhile (<= m)
              $ scanl (+) 0
              $ cycle
              $ toStds d
      in DurationS $ take k $ cycle d

    toStds
      = map (fromIntegral . sum . map durToMinutes . takeDurations)

    takeDurations (_,l) = map (\(_,d,_) -> d) l

    truncMaximum d c = do
      me <- getMaximumEndpoint
      let c' = stdCycle (Just c)
          k = floor $ c' * me
      putStrLn $ "=========== k = " ++ show k ++ " ==========="
      return $ DurationS $ take k $ cycle d

    getMaximumEndpoint = do
      now <- getCurrentTime
      let end = addUTCTime oneYear now
          oneYear = fromIntegral ((durToMinutes (Months 12)) * 60)
          t = (diffUTCTime end now) / 60
      return $ realToFrac t

----- Asserting
    
assert :: (Show a,Eq a) => a -> a -> String -> IO ()
assert a b formula
  | a == b    = return ()
  | otherwise =
    error
    $ show a
    ++ " different than "
    ++ show b
    ++ " in "
    ++ formula ++ "."

-------------------- Helper Functions

len :: DurStd -> Double
len = fromIntegral . length

alwaysDur =
  DurationInfS
  . (\x -> [(Nothing, x)])
  . (\x -> [(Nothing,x,NoInterval)])
  . Minutes

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

substOrDelete :: (a -> Bool) -> Maybe a -> [a] -> [a]
substOrDelete _ _ [] = []
substOrDelete pred x' (x:xs)
  | pred x && isNothing x' =          substOrDelete pred x' xs
  | pred x                 = frj x' : substOrDelete pred x' xs
  | otherwise              = x      : substOrDelete pred x' xs

-- fromJust
frj :: Maybe a -> a
frj (Just x) = x

---------------------------------- Teste

-- teste da funçao mergeStates
teste = mergeStates
  [("itsme", State Nothing (Just (TimeSpent 5)))
  ,("heheh", State (Just (StepsDone 1)) Nothing)]
  [("heheh", ProjectS
             Nothing
             (
               Just
              (
                DurationInfS
               [
                 (
                   Nothing,
                   [(Nothing,Minutes 5, NoInterval),
                    (Nothing,Minutes 4, NoInterval),
                    (Nothing,Minutes 3, NoInterval)]
                ),(
                   Nothing,
                   [(Nothing,Minutes 10, NoInterval),
                    (Nothing,Minutes 8, NoInterval),
                    (Nothing,Minutes 6, NoInterval)]
                )
               ]
              )
             )
             Nothing
             Nothing
             restNo)
  ,("itsme", ProjectS Nothing Nothing Nothing (Just (Totaltime 6)) restNo)] where
    restNo = RestS Nothing Nothing Nothing [] []

ts = stdDur (Just (DurationInfS
               [
                 (
                   Nothing,
                   [(Nothing,Hours 3, NoInterval),
                    (Nothing,Months 3, NoInterval)]
                )
               ]))

tst = do
  zone <- getZoneIO
  end <- stdEndpoint
         (Just
          (Endpoint (fromGregToZoned zone (2016,3,16,12,0,0))))
  let c' = stdCycle (Just (CycleS 1 (Weeks 1)))
      k = c' * end
  print k
  putStrLn "--------------"
  return $ estimateDT_CE c' end
