{-# LANGUAGE ViewPatterns #-}

module ProjectS where

-------------------- Imports

import Data
import Data.List hiding (cycle)
import Data.Time hiding (toGregorian, fromGregorian)
import qualified Data.Time.Calendar as C
import Data.Time.Calendar.WeekDate
import qualified Data.DateTime as D
import Data.DateTime hiding (getCurrentTime)

-------------------- Command And Project Type

commandType :: Command -> CommandType
commandType comm = case comm of
  CycleC _ _            -> C

  DurationC _           -> D'
  DurationsC _          -> D
  DurationsiC _         -> D'
  DurationsnC _         -> D
  DurationsniC _        -> D'

  DivisionC _           -> D
  DivisionnC _          -> D
  DivisionsC _          -> D
  DivisionsiC _         -> D'
  DivisionsnC _         -> D
  DivisionsniC _        -> D'
  DivisionsnnC _        -> D
  DivisionsnniC _       -> D'
  
  TodayC                -> E
  EndpointC _           -> E
  
  TotaltimeC _          -> T
  EffectiveTotaltimeC _ -> T
  
  NotinC _              -> ICons
  InC _                 -> ICons
  NotbeginC _           -> ICons
  BeginC _              -> ICons
  NotendC _             -> ICons
  EndC _                -> ICons
  
  AfterC _              -> ConsT
--  BeforeC _             -> ConsT
  RightafterC _         -> ConsT
-- RightbeforeC _        -> ConsT
  
  AtC _                 -> PlaceT
  
  MsgC _                -> MsgT
  MsgsC _               -> MsgT

projectTypeAndRestFromCommandTypes ::
  [CommandType] -> (ProjectType,[CommandType])
projectTypeAndRestFromCommandTypes cst = case sort cst of
  C : D : E : T : s -> (CDET , s)
  C : D': E : T : s -> (CD'ET, s)
  C : D : E : s     -> (CDE  , s)
  C : D': E : s     -> (CD'E , s)
  C : D : T : s     -> (CDT  , s)
  C : D': T : s     -> (CD'T , s)
  C : E : T : s     -> (CET  , s)
  D : E : T : s     -> (DET  , s)
  D': E : T : s     -> (D'ET , s)
  C : D : s         -> (CD   , s)
  C : D': s         -> (CD'  , s)
  D : E : s         -> (DE   , s)
  C : E : s         -> (CE   , s)
  E : T : s         -> (ET   , s)
  C : T : s         -> (CT   , s)
  _                 -> error "Project not well defined."

projectTypeFromCommandTypes :: [CommandType] -> ProjectType
projectTypeFromCommandTypes =
  fst . projectTypeAndRestFromCommandTypes

restOfCommandTypes :: [CommandType] -> [CommandType]
restOfCommandTypes = snd . projectTypeAndRestFromCommandTypes

projectType :: [Command] -> ProjectType
projectType = projectTypeFromCommandTypes . map commandType

checkProjectTypeErrors :: [Command] -> ProjectType -> ProjectType
checkProjectTypeErrors cs pType =
  case (checkDuplicates cTypes pType) of
    Ok      -> pType
    Err err -> error $ err ++ "."
  where
    cTypes = restOfCommandTypes $ map commandType cs

    checkDuplicates :: [CommandType] -> ProjectType -> OkErr
    checkDuplicates cTypes pType = has listOfDups cTypes

    listOfDups = case pType of
      CD    -> [D,D']
      CD'   -> [D,D']
      DE    -> [E]
      CE    -> [E]
      ET    -> [T]
      CT    -> [T]
      CDE   -> [E]
      CD'E  -> [E]
      CDT   -> [T]
      CD'T  -> [T]
      CET   -> [T]
      DET   -> [T]
      D'ET  -> [T]
      CDET  -> [T]
      CD'ET -> [T]
         
    hasBool :: [CommandType] -> [CommandType] -> Bool
    hasBool s [] = False
    hasBool s cs = or $ map (\g -> or $ map (==g) cs) s

    has  :: [CommandType] -> [CommandType] -> OkErr
    has cts = fromBoolToOkErr . hasBool cts

    fromBoolToOkErr :: Bool -> OkErr
    fromBoolToOkErr True  =
      Err "Duplicated cycle, duration, endpoint or totaltime"
    fromBoolToOkErr False = Ok

projectTypeHandlingErrors :: [Command] -> ProjectType
projectTypeHandlingErrors cs =
  checkProjectTypeErrors cs (projectType cs)

-------------------- Project Creation

makeProject :: [Command] -> IO ProjectS
makeProject csUnsorted = case projectTypeHandlingErrors cs of
  CD    -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    as <- rest (drop 2 cs)
    return $ ProjectS (Just c) (Just d) Nothing Nothing as
  CD'   -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    as <- rest (drop 2 cs)
    return $ ProjectS (Just c) (Just d) Nothing Nothing as
  DE    -> do
    let d = duration (cs !! 0)
    e  <- endpoint (cs !! 1)
    as <- rest (drop 2 cs)
    return $ ProjectS Nothing (Just d) (Just e) Nothing as
  CE    -> do
    let c = cycl (cs !! 0)
    e  <- endpoint (cs !! 1)
    as <- rest (drop 2 cs)
    return $ ProjectS (Just c) Nothing (Just e) Nothing as
  ET    -> do
    e <- endpoint (cs !! 0)
    let t = totaltime (cs !! 1)
    as <- rest (drop 2 cs) 
    return $ ProjectS Nothing Nothing (Just e) (Just t) as
  CT    -> do
    let c = cycl (cs !! 0)
        t = totaltime (cs !! 1)
    as <- rest (drop 2 cs) 
    return $ ProjectS (Just c) Nothing Nothing (Just t) as
  CDE   -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e  <- endpoint (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ ProjectS (Just c) (Just d) (Just e) Nothing as
  CD'E  -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e  <- endpoint (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ ProjectS (Just c) (Just d) (Just e) Nothing as
  CDT   -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
        t = totaltime (cs !! 2)
    as <- rest (drop 3 cs)
    return $ ProjectS (Just c) (Just d) Nothing (Just t) as
  CD'T  -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
        t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ ProjectS (Just c) (Just d) Nothing (Just t) as
  CET   -> do
    let c = cycl (cs !! 0)
    e  <- endpoint (cs !! 1)
    let t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ ProjectS (Just c) Nothing (Just e) (Just t) as
  DET   -> do
    let d = duration (cs !! 0)
    e <- endpoint (cs !! 1)
    let t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ ProjectS Nothing (Just d) (Just e) (Just t) as
  D'ET  -> do
    let d = duration (cs !! 0)
    e  <- endpoint (cs !! 1)
    let t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ ProjectS Nothing (Just d) (Just e) (Just t) as
  CDET  -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e  <- endpoint (cs !! 2)
    let t = totaltime (cs !! 3)
    as <- rest (drop 4 cs) 
    return $ ProjectS (Just c) (Just d) (Just e) (Just t) as
  CD'ET -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e  <- endpoint (cs !! 2)
    let t = totaltime (cs !! 3)
    as <- rest (drop 4 cs) 
    return $ ProjectS (Just c) (Just d) (Just e) (Just t) as
  where
    cs = sortComm csUnsorted

    sortComm :: [Command] -> [Command]
    sortComm cs =
      map fst
      $ sortBy (\(_,t) (_,t') -> compare t t')
      $ zip cs (map commandType cs)

----- Cycle, Duration, Endpoint, Totaltime Construction
    
cycl :: Command -> CycleS
cycl (CycleC num time) = CycleS num num time --(durToMinutes time)
cycl _ = error "cycl: Wrong command type."

duration :: Command -> DurationS
duration comm = case comm of
  DurationC tim ->
    cycle $ [(Nothing,[(Nothing, tim, NoInterval)])]
  DurationsC tims ->
    dur $ map
    (\tim -> (Nothing, [(Nothing, tim, NoInterval)]))
    tims
  DurationsiC tims ->
    cycle $ map
    (\tim -> (Nothing, [(Nothing, tim, NoInterval)]))
    tims
  DurationsnC nameNtims ->
    dur $ map
    (\(name,tim) -> (Just name, [(Nothing, tim, NoInterval)]))
    nameNtims
  DurationsniC nameNtims ->
    cycle $ map
    (\(name,tim) -> (Just name, [(Nothing, tim, NoInterval)]))
    nameNtims
    
  DivisionC timNints ->
    dur
    [(Nothing, map (\(tim,int) -> (Nothing, tim, int)) timNints)]
  DivisionnC nameNtimNints ->
    dur [(Nothing, map (\(n,t,i) -> (Just n,t,i)) nameNtimNints)]
  DivisionsC timNintsList ->
    dur $ map
    (\timNints ->
      (Nothing,
       map
       (\(tim,int) -> (Nothing, tim, int))
       timNints))
    timNintsList
  DivisionsiC timNintsList ->
    cycle $ map
    (\timNints ->
      (Nothing,
       map
       (\(tim,int) -> (Nothing, tim, int))
       timNints))
    timNintsList
  DivisionsnC nameNtimNintsList ->
    dur $ map
    (\nameNtimNints ->
      (Nothing,
       map
       (\(name,tim,int) -> (Just name,tim,int))
       nameNtimNints))
    nameNtimNintsList
  DivisionsniC nameNtimNintsList ->
    cycle $ map
    (\nameNtimNints ->
      (Nothing,
       map
       (\(name,tim,int) -> (Just name,tim,int))
       nameNtimNints))
    nameNtimNintsList
  DivisionsnnC nameNtimNintsList ->
    dur $ map
    (\(name,nameNtimNints) ->
      (Just name,
       map
       (\(name',tim,int) -> (Just name',tim,int))
       nameNtimNints))
    nameNtimNintsList
  DivisionsnniC nameNtimNintsList ->
    cycle $ map
    (\(name,nameNtimNints) ->
      (Just name,
       map
       (\(name',tim,int) -> (Just name',tim,int))
       nameNtimNints))
    nameNtimNintsList
  _ -> error "duration: Wrong command type."
  where dur   = DurationS
        cycle = DurationInfS

endpoint :: Command -> IO Endpoint
endpoint endp = case endp of
  EndpointC inst -> Endpoint <$> fromInstantToCompleteTime inst
  TodayC -> Endpoint <$> todayEndpoint
  _ -> error "endpoint: Wrong command type."
  where
    todayEndpoint :: IO ZonedTime
    todayEndpoint = do
      date <- getZonedTime
      let zone = getZone date
          (y,m,d,_,_,_) = fromZonedToGreg date
      return $ fromGregToZoned zone (y,m,d+1,0,0,0)
      -- the project that ends today has endpoint
      -- at the beggining of the next day

totaltime :: Command -> Totaltime
totaltime tot = case tot of
  TotaltimeC time -> Totaltime $ durToMinutes time
  EffectiveTotaltimeC time -> EffectiveTotaltime $ durToMinutes time
  _ -> error "totaltime: Wrong command type."

----- Rest Construction

rest :: [Command] -> IO RestS
rest comms =
  RestS
  <$> intervalConstraint comms
  <*> (return (place  comms))
  <*> (return (messg  comms))
  <*> (return (constr comms))
  <*> (return (costs  comms))

-- Interval Constraint

intervalConstraint :: [Command] -> IO (Maybe IntervalConstraintS)
intervalConstraint = iCons . filter ((== ICons) . commandType)
  where
    iCons []  = return Nothing
    iCons [x] = Just <$> commandToICons x
    iCons   _ = error "More than one interval constraint defined."

    commandToICons c = case c of
      NotinC    spec -> NotinS    <$> completeTimes spec
      InC       spec -> InS       <$> completeTimes spec
      NotbeginC spec -> NotbeginS <$> completeTimes spec
      BeginC    spec -> BeginS    <$> completeTimes spec
      NotendC   spec -> NotendS   <$> completeTimes spec
      EndC      spec -> EndS      <$> completeTimes spec

    completeTimes :: SpecialInterval -> IO SpecialInterval
    completeTimes int = case int of
      Interval i ->
        Complete <$> propag (fmap fromInstantToCompleteTime i)
      And a b -> And <$> completeTimes a <*> completeTimes b
      Or  a b -> Or  <$> completeTimes a <*> completeTimes b
      x -> return x

    propag :: Monad m => Interval (m a) -> m (Interval a)
    propag int = case int of
      FromTo fr to -> do
        fr' <- fr
        to' <- to
        return (FromTo fr' to')
      OrMore t -> do
        t'  <-  t
        return (OrMore t')
      Exact  t -> do
        t'  <-  t
        return (Exact  t')
      NoInterval -> return NoInterval
      

-- Place

place :: [Command] -> Maybe Place
place = place' . filter ((== PlaceT) . commandType)
  where
    place' :: [Command] -> Maybe Place
    place'  [] = Nothing
    place' [x] = Just (commandToPlace x)
    place'   _ = error "More than one place defined."

    commandToPlace :: Command -> Place
    commandToPlace (AtC name) = At name

-- Messages

messg :: [Command] -> Maybe Messg
messg = messg' . filter ((== MsgT) . commandType)
  where
    messg' :: [Command] -> Maybe Messg
    messg'  [] = Nothing
    messg' [x] = Just (commandToMessg x)
    messg'   _ = error "More than one message defined."

    commandToMessg :: Command -> Messg
    commandToMessg (MsgC   msg) = Msg  msg
    commandToMessg (MsgsC msgs) = Msgs msgs

-- Constraints

constr :: [Command] -> [Constraint]
constr = map commandToConstr . filter ((== ConsT) . commandType)
  where
    commandToConstr :: Command -> Constraint
    commandToConstr comm = case comm of
      AfterC       name -> After       name
--      BeforeC      name -> Before      name
      RightafterC  name -> RightAfter  name
--      RightbeforeC name -> RightBefore name

-- Costs

costs :: [Command] -> [Cost]
costs = map commandToCosts . filter ((== CosT) . commandType)
  where
    commandToCosts :: Command -> Cost
    commandToCosts _ = Cst

-------------------- Time Helper Functions

fromInstantToCompleteTime :: Instant -> IO ZonedTime
fromInstantToCompleteTime inst = case inst of
  PutYearAndMonth date -> putYearAndMonth date
  PutYear         date -> putYear         date
  PutToday        date -> putToday        date
  CompleteTime    date -> return          date

putTemplate
  :: (Gregorian -> Gregorian -> Gregorian)
  -> ZonedTime -> IO ZonedTime
putTemplate f time = do
  date <- getZonedTime
  let zone = getZone time
      greg = f (fromZonedToGreg date) (fromZonedToGreg time)
  return $ fromGregToZoned zone greg

putYearAndMonth :: ZonedTime -> IO ZonedTime
putYearAndMonth = putTemplate yearAndMonth where
    yearAndMonth (y,mo,_,_,_,_) (_, _,d,h,m,s) = (y,mo,d,h,m,s)

putYear :: ZonedTime -> IO ZonedTime
putYear = putTemplate year where
  year (y,_,_,_,_,_) (_,mo,d,h,m,s) = (y,mo,d,h,m,s)

putToday :: ZonedTime -> IO ZonedTime
putToday = putTemplate year where
  year (y,mo,d,_,_,_) (_,_,_,h,m,s) = (y,mo,d,h,m,s)

-- Duration Functions

durToMinutes :: Dur -> Minutes
durToMinutes dur = case dur of
  Minutes  m -> m
  Hours    h -> h*60
  Days     d -> d*1440
  Weeks    w -> w*10080  -- 60 * 24 * 7
  Months  mo -> mo*44640 -- 60 * 24 * 31
  Plus   x y -> durToMinutes x + durToMinutes y

-- ZonedTime Manipulation Functions

getZoneIO :: IO (TimeZone, Minutes)
getZoneIO = getZone <$> getZonedTime

getZone :: ZonedTime -> (TimeZone, Minutes)
getZone (ZonedTime _ timeZone@(TimeZone mins _ _)) = (timeZone,mins)

fromGregToZoned :: (TimeZone, Minutes) -> Gregorian -> ZonedTime
fromGregToZoned (timeZone, offset) (y,mo,d,h,m,s) =
  utcToZonedTime timeZone
  $ addMinutes (-(fromIntegral offset))
  $ fromGregorian y mo d h m s

fromZonedToGreg :: ZonedTime -> (Integer, Int, Int, Int, Int,Int)
fromZonedToGreg time@(ZonedTime _ timeZone@(TimeZone mins _ _)) =
  toGregorian $ addMinutes (fromIntegral mins) (zonedTimeToUTC time)

fromGregToZonedIO :: Gregorian -> IO ZonedTime
fromGregToZonedIO greg = do
  zone <- getZoneIO
  return $ fromGregToZoned zone greg

fromZonedToWeekDate :: ZonedTime -> (Integer,Int,Int)
fromZonedToWeekDate time =
  let (y,mo,d,_,_,_) = fromZonedToGreg time
  in toWeekDate $ C.fromGregorian y mo d

fromWeekDateToZoned
  :: (TimeZone,Minutes) -> (Integer,Int,Int) -> ZonedTime
fromWeekDateToZoned zone (y',w',d') =
  let (y,mo,d) = C.toGregorian $ fromWeekDate y' w' d'
  in fromGregToZoned zone (y,mo,d,0,0,0)
