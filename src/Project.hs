module Project where

import System.Directory
import Data

getState :: IO [(Name,State)] -- [(Name, State)]
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
  
-- funcao feia (bem feia)
mergeStates
  :: [(Name, State)] -> [(Name, ProjectS)] -> [(Name, ProjectS)]
mergeStates states projs = case states of
  []   -> projs
  s:ss -> mergeStates ss (mergeState s projs)
  where

    mergeState
      :: (Name, State)
      -> [(Name, ProjectS)]
      -> [(Name, ProjectS)]
    mergeState (name, state) projects
      = case filter (\(n,_) -> n == name) projects of
        [] -> error $ "No project with name " ++ name ++ "."
        [p@(n,_)] ->
          substOrDelete
          (\(n,_) -> n == name)
          (merge p state)
          projects
        _ -> error $ "More than one project with name " ++ name ++ "."

    substOrDelete :: (a -> Bool) -> Maybe a -> [a] -> [a]
    substOrDelete _ _ [] = []
    substOrDelete pred x' (x:xs)
      | pred x && isNothing x' =         substOrDelete pred x' xs
      | pred x                 = fj x' : substOrDelete pred x' xs
      | otherwise              = x     : substOrDelete pred x' xs

    fj (Just x) = x

    isNothing Nothing = True
    isNothing _ = False
    
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

    merge :: (Name,ProjectS) -> State -> Maybe (Name,ProjectS)
    merge (name, ProjectS c d e t r) (State n ts)
      | stepsDone n  d == Finished ||
        timeSpent ts t == Finished = Nothing
      | otherwise = let Unfinished d' = stepsDone n d
                        Unfinished t' = timeSpent ts t
                    in Just (name, ProjectS c d' e t' r)
  
    stepsDone
      :: Maybe StepsDone
      -> Maybe DurationS
      -> IsFinished (Maybe DurationS)
    stepsDone Nothing d = Unfinished d
    stepsDone _ Nothing = Unfinished Nothing
    stepsDone (Just (StepsDone 0)) jd = Unfinished jd
    stepsDone n (Just (DurationInfS d)) =
      stepsDone n (Just (DurationS (cycle d)))
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
      :: Maybe TimeSpent -> Maybe Totaltime -> IsFinished (Maybe Totaltime)
    timeSpent Nothing t = Unfinished t
    timeSpent _ Nothing = Unfinished Nothing
    timeSpent (Just (TimeSpent timeS)) (Just (Totaltime tot))
      | tot - timeS <= 0 = Finished
      | otherwise        = Unfinished $ Just $ Totaltime $ tot - timeS
    timeSpent
      (Just (TimeSpent timeS))
      (Just (EffectiveTotaltime tot))
      | tot - timeS <= 0 = Finished
      | otherwise        =
        Unfinished $ Just $ EffectiveTotaltime $ tot - timeS


----------------------------------------
-- teste da funçao mergeStates
teste = mergeStates
  [("itsme", State Nothing (Just (TimeSpent 5)))
  ,("heheh", State (Just (StepsDone 1)) Nothing)]
  [("heheh", ProjectS
             Nothing
             (
               Just
              (
                DurationS
               [
                 (
                   Nothing,
                   [(Nothing,Minutes 3, NoInterval),
                    (Nothing,Minutes 3, NoInterval)]
                )
               ]
              )
             )
             Nothing
             Nothing
             restNo)
  ,("itsme", ProjectS Nothing Nothing Nothing (Just (Totaltime 6)) restNo)] where
    restNo = RestS Nothing Nothing Nothing [] []
------------------------------------

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

infer :: (ProjectType, ProjectS) -> ProjectS
infer pType (Project c d e t r) = case pType of
  CD    -> let c' = 
  CD'   -> asdasd
  DE    -> asdasd
  CE    -> asdasd
  ET    -> asdasd
  CT    -> asdasd
  CDE   -> asdasd
  CD'E  -> asdasd
  CDT   -> asdasd
  CD'T  -> asdasd
  CET   -> asdasd
  DET   -> asdasd
  D'ET  -> asdasd
  CDET  -> asdasd
  CD'ET -> asdasd
