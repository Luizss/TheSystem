module Project where

-------------------- Imports

import System.Directory
import Data
import ProjectS

import Data.Time
import Data.Time.Clock

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
mergeStates     [] projs = projs
mergeStates (s:ss) projs = mergeStates ss (mergeState s projs)

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
      -> Maybe (Totaltime Minutes)
      -> IsFinished (Maybe (Totaltime Minutes))
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

infer
  :: ProjectType -> ProjectS -> IO (CycleS, DurationS, Totaltime Double)
infer pType (ProjectS c d e t r) = case pType of
  CD    -> do
    let d' = stdDur d
    return (frj c, frj d, inferTotaltime d')
  CD'   -> do
    let d' = stdDur d
    return (frj c, frj d, Infinity)
  DE    -> do
    let d' = stdDur d
        k  = length d'
    e' <- stdEndpoint e
    return (inferCycle k e, frj d, inferTotaltime d')
  CE    -> do
    let c' = stdCycle c
    e' <- stdEndpoint e
    let (d,t) = estimate c' e'
    return (frj c,undefined,undefined)
  ET    -> do
    let t' = t
    e' <- stdEndpoint e
    return (undefined,undefined,frj' t)
  CT    -> do
    let c' = stdCycle c
        t' = t
    return (frj c,undefined ,frj' t)
  CDE   -> do
    let c' = stdCycle c
        d' = stdDur d
    e' <- stdEndpoint e
    return (frj c, frj d,undefined)
  CD'E  -> do
    let c' = stdCycle c
        d' = stdDur d
    e' <- stdEndpoint e
    return (frj c, frj d,undefined)
  CDT   -> do
    let c' = stdCycle c
        d' = stdDur d
        t' = t
    return (frj c, frj d, frj' t)
  CD'T  -> do
    let c' = stdCycle c
        d' = stdDur d
        t' = t
    return (frj c, frj d, frj' t)
  CET   -> do
    let c' = stdCycle c
    e' <- stdEndpoint e
    let t' = t
    return (frj c,undefined , frj' t)
  DET   -> do
    let d' = stdDur d
    e' <- stdEndpoint e
    let t' = t
    return (undefined, frj d, frj' t)
  D'ET  -> do
    let d' = stdDur d
    e' <- stdEndpoint e
    let t' = t
    return (undefined, frj d, frj' t)
  CDET  -> do
    let c' = stdCycle c
        d' = stdDur d
    e' <- stdEndpoint e
    let t' = t
    return (frj c, frj d, frj' t)
  CD'ET -> do
    let c' = stdCycle c
        d' = stdDur d
    e' <- stdEndpoint e
    let t' = t
    return (frj c, frj d, frj' t)
  where frj' (Just (Totaltime x))
          = Totaltime (fromIntegral x)
        frj' (Just (EffectiveTotaltime x))
          = EffectiveTotaltime (fromIntegral x)
        
inferCycle = undefined
inferTotaltime = Totaltime . fromIntegral . sum

alwaysDur =
  DurationInfS
  . (\x -> [(Nothing, x)])
  . (\x -> [(Nothing,x,NoInterval)])
  . Minutes
  . (60 *)
  
estimate :: Double -> Minutes -> (DurationS,Double)
estimate cycle minutesTillEnd
  = ( alwaysDur 2
    , cycle * fromIntegral minutesTillEnd * 2 * 60)

roundAt2 :: Double -> Minutes
roundAt2 = (\x -> if odd x then x - 1 else x) . ceiling

tst = do
  zone <- getZoneIO
  end <- stdEndpoint
         (Just
          (Endpoint (fromGregToZoned zone (2016,4,2,3,0,0))))
  let c' = stdCycle (Just (CycleS 2 (durToMinutes (Weeks 2))))
  return $ estimate c' end
  
stdCycle :: Maybe CycleS -> Double
stdCycle (Just (CycleS times mins))
  = fromIntegral times / fromIntegral mins

stdDur :: Maybe DurationS -> [Minutes] --[[Minutes]]
stdDur (Just d) = case d of
  DurationS lst ->
    map
    (sum . map durToMinutes . takeDurations)
    lst
  DurationInfS lst ->
    map
    (sum . map durToMinutes . takeDurations)
    lst
  where takeDurations (_,l) = map (\(_,d,_) -> d) l

stdEndpoint :: Maybe Endpoint -> IO MinutesD
stdEndpoint (Just (Endpoint end)) = do
  now <- getCurrentTime
  let a = ceiling ((diffUTCTime (zonedTimeToUTC end) now) / 60)
          :: Minutes
  return a



{-inferEndpoint :: Double -> Int -> IO Endpoint
inferEndpoint c k = do
  now <- getCurrentTime
  let timeLeft = (fromIntegral k) / c
      a = fromRational (toRational ((floor timeLeft) * 60))
          :: NominalDiffTime
      end = addUTCTime a now
  endp <- utcToLocalZonedTime end
  return $ Endpoint endp-}

--- ? Sum ?


-------------------------- Helper Functions

asdasd = undefined

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

substOrDelete :: (a -> Bool) -> Maybe a -> [a] -> [a]
substOrDelete _ _ [] = []
substOrDelete pred x' (x:xs)
  | pred x && isNothing x' =          substOrDelete pred x' xs
  | pred x                 = frj x' : substOrDelete pred x' xs
  | otherwise              = x      : substOrDelete pred x' xs

fromJust :: Maybe a -> a
fromJust (Just x) = x

frj :: Maybe a -> a
frj = fromJust

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
