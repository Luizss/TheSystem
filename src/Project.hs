{-# LANGUAGE ViewPatterns #-}

module Project where

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

  DurationC _           -> D
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
  BeforeC _             -> ConsT
  RightafterC _         -> ConsT
  RightbeforeC _        -> ConsT
  
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
    fromBoolToOkErr True  = Ok
    fromBoolToOkErr False =
      Err "Duplicated cycle, duration, endpoint or totaltime"

projectTypeHandlingErrors :: [Command] -> ProjectType
projectTypeHandlingErrors cs =
  checkProjectTypeErrors cs (projectType cs)

-------------------- Project Creation

makeProject :: [Command] -> IO Project
makeProject csUnsorted = case projectTypeHandlingErrors cs of
  CD    -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    as <- rest (drop 2 cs)
    return $ Project (Just c) (Just d) Nothing Nothing as
  CD'   -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    as <- rest (drop 2 cs)
    return $ Project (Just c) (Just d) Nothing Nothing as
  DE    -> do
    let d = duration (cs !! 0)
    e  <- endpoint (cs !! 1)
    as <- rest (drop 2 cs)
    return $ Project Nothing (Just d) (Just e) Nothing as
  CE    -> do
    let c = cycl (cs !! 0)
    e  <- endpoint (cs !! 1)
    as <- rest (drop 2 cs)
    return $ Project (Just c) Nothing (Just e) Nothing as
  ET    -> do
    e <- endpoint (cs !! 0)
    let t = totaltime (cs !! 1)
    as <- rest (drop 2 cs) 
    return $ Project Nothing Nothing (Just e) (Just t) as
  CT    -> do
    let c = cycl (cs !! 0)
        t = totaltime (cs !! 1)
    as <- rest (drop 2 cs) 
    return $ Project (Just c) Nothing Nothing (Just t) as
  CDE   -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e <- endpoint (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ Project (Just c) (Just d) (Just e) Nothing as
  CD'E  -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e <- endpoint (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ Project (Just c) (Just d) (Just e) Nothing as
  CDT   -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
        t = totaltime (cs !! 2)
    as <- rest (drop 3 cs)
    return $ Project (Just c) (Just d) Nothing (Just t) as
  CD'T  -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
        t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ Project (Just c) (Just d) Nothing (Just t) as
  CET   -> do
    let c = cycl (cs !! 0)
    e <- endpoint (cs !! 1)
    let t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ Project (Just c) Nothing (Just e) (Just t) as
  DET   -> do
    let d = duration (cs !! 0)
    e <- endpoint (cs !! 1)
    let t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ Project Nothing (Just d) (Just e) (Just t) as
  D'ET  -> do
    let d = duration (cs !! 0)
    e <- endpoint (cs !! 1)
    let t = totaltime (cs !! 2)
    as <- rest (drop 3 cs) 
    return $ Project Nothing (Just d) (Just e) (Just t) as
  CDET  -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e <- endpoint (cs !! 2)
    let t = totaltime (cs !! 3)
    as <- rest (drop 4 cs) 
    return $ Project (Just c) (Just d) (Just e) (Just t) as
  CD'ET -> do
    let c = cycl (cs !! 0)
        d = duration (cs !! 1)
    e <- endpoint (cs !! 2)
    let t = totaltime (cs !! 3)
    as <- rest (drop 4 cs) 
    return $ Project (Just c) (Just d) (Just e) (Just t) as
  where
    cs = sortComm csUnsorted

    sortComm :: [Command] -> [Command]
    sortComm cs =
      map fst
      $ sortBy (\(_,t) (_,t') -> compare t t')
      $ zip cs (map commandType cs)

---------- Cycle, Duration, Endpoint, Totaltime Construction
    
cycl :: Command -> Cycle
cycl (CycleC num time) = Cycle num (durToSeconds time)
cycl _ = error "cycl: Wrong command type."

duration :: Command -> Duration
duration comm = Duration $ case comm of
  DurationC tim ->
    [(Nothing,[(Nothing, tim, NoInterval)])]
  DurationsC tims ->
    map
    (\tim -> (Nothing, [(Nothing, tim, NoInterval)]))
    tims
  DurationsiC tims ->
    cycle $ map
    (\tim -> (Nothing, [(Nothing, tim, NoInterval)]))
    tims
  DurationsnC nameNtims ->
    map
    (\(name,tim) -> (Just name, [(Nothing, tim, NoInterval)]))
    nameNtims
  DurationsniC nameNtims ->
    cycle $ map
    (\(name,tim) -> (Just name, [(Nothing, tim, NoInterval)]))
    nameNtims
    
  DivisionC timNints ->
    [(Nothing, map (\(tim,int) -> (Nothing, tim, int)) timNints)]
  DivisionnC nameNtimNints ->
    [(Nothing, map (\(n,t,i) -> (Just n,t,i)) nameNtimNints)]
  DivisionsC timNintsList ->
    map
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
    map
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
    map
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

endpoint :: Command -> IO Endpoint
endpoint (EndpointC inst) =
  Endpoint <$> fromInstantToCompleteTime inst
endpoint TodayC = Endpoint <$> today
endpoint _ = error "endpoint: Wrong command type."

totaltime :: Command -> Totaltime
totaltime (TotaltimeC time) =
  Totaltime $ durToSeconds time
totaltime (EffectiveTotaltimeC time) =
  EffectiveTotaltime $ durToSeconds time
totaltime _ = error "totaltime: Wrong command type."

---------- Rest Construction:
---------- IntervalConstraint,Place,Messages,Constraints,Costs

rest :: [Command] -> IO Rest
rest comms =
  Rest
  <$> intervalConstraint comms
  <*> return (place  comms)
  <*> return (messg  comms)
  <*> return (constr comms)
  <*> return (costs  comms)

------------------------- Working Area

-- Checar se as funçoes de tempo com ZonedTime realmente mostram o zonedtime certo

--- Fazer as funçoes gregorian para zonedtime

getZone :: IO (TimeZone, Minutes)
getZone = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  return (timeZone, fromIntegral mins)

weekday y mo d = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  print =<< fromGregToZonedIO y mo d 0 0
  return
    $ toWeekDate
    $ C.fromGregorian y mo d

fromGregToZoned
  :: TimeZone -> Minutes
  -> Integer -> Int -> Int -> Int -> Int -> ZonedTime
fromGregToZoned timeZone offset y mo d h m =
    utcToZonedTime timeZone
    $ fromGregorian y mo d h (m-offset) 0

fromGregToZonedIO
  :: Integer -> Int -> Int -> Int -> Int -> IO ZonedTime
fromGregToZonedIO y mo d h m = do
  (timeZone,offset) <- getZone
  return $ fromGregToZoned timeZone offset y mo d h m

fromZonedToGreg :: ZonedTime -> (Integer, Int, Int, Int, Int,Int)
fromZonedToGreg time@(ZonedTime _ timeZone@(TimeZone mins _ _)) =
  toGregorian $ addMinutes (fromIntegral mins) (zonedTimeToUTC time)

--type Head = Interval ZonedTime
type Minutes = Int
type Hours = Integer

minimumIntervalDuration :: Minutes
minimumIntervalDuration = 6 * 60

makeIntervals
  :: (ZonedTime -> Bool)
  -> Dur -- Duration of Intervals
  -> Dur -- Period between Intervals
  -> IO [Interval ZonedTime]
makeIntervals isIn dur per = do
  now <- getZonedTime

  let
    
    head = FromTo now seed
    
    seed = case isIn now of
      True  -> getSeedAfterTrue now
      False -> getSeed now

    rest = makeRestFromSeed seed
    
  return $ glueHeadWithRest head rest
  where
    
    durMin = durToMinutes dur
    
    getSeed :: ZonedTime -> ZonedTime
    getSeed now =
      let next = addTime durMin now
      in case isIn next of
        True  -> roundAtBegginingOfInterval now
        False -> getSeed next

    getSeedAfterTrue :: ZonedTime -> ZonedTime
    getSeedAfterTrue now =
      let next = addTime durMin now 
      in case isIn next of
        True  -> getSeedAfterTrue next
        False -> getSeed next

    addTime :: Minutes -> ZonedTime -> ZonedTime
    addTime amount date@(ZonedTime _ timeZone) =
      utcToZonedTime timeZone
      $ addMinutes (fromIntegral amount)
      $ zonedTimeToUTC date
      
    roundAtBegginingOfInterval :: ZonedTime -> ZonedTime
    roundAtBegginingOfInterval = case dur of   
      Hours  6 -> roundAt6Hours
      Days   1 -> roundAtDay 
      Weeks  1 -> roundAtWeek
      Months 1 -> roundAtMonth

    roundAt6Hours :: ZonedTime -> ZonedTime
    roundAt6Hours time@(ZonedTime _ timeZone@(TimeZone mins _ _)) =
      let (y,mo,d,h,_,_) =
            toGregorian
            $ addMinutes
            (-(fromIntegral mins))
            (zonedTimeToUTC time)
          rounded :: Int
          rounded
            | h >= 0  && h < 6   = 0
            | h >= 6  && h < 12  = 6 
            | h >= 12 && h < 18  = 12
            | h >= 18 && h <= 23 = 18
      in utcToZonedTime timeZone
         $ fromGregorian y mo d rounded mins 0

    -- TODO 1
    roundAtDay = undefined
    roundAtWeek = undefined
    roundAtMonth = undefined

    -- TODO 2
    makeRestFromSeed :: ZonedTime -> [Interval ZonedTime]
    makeRestFromSeed z = undefined

    -- TODO 3
    glueHeadWithRest = undefined

{-roundAt6Hours :: ZonedTime -> ZonedTime
roundAt6Hours time@(ZonedTime _ timeZone@(TimeZone mins _ _)) =
  let utc = zonedTimeToUTC time
      offset = -(fromIntegral mins)
      (y,mo,d,h,_,_) = toGregorian $ addMinutes offset utc
      rounded :: Int
      rounded
        | h >= 0  && h < 6   = 0
        | h >= 6  && h < 12  = 6 
        | h >= 12 && h < 18  = 12
        | h >= 18 && h <= 23 = 18
  in utcToZonedTime timeZone
     $ fromGregorian y mo d rounded mins 0-}

roundAt6Hours :: ZonedTime -> ZonedTime
roundAt6Hours time@(ZonedTime _ timeZone@(TimeZone mins _ _)) =
  let (y,mo,d,h,_,_) = fromZonedToGreg time
      rounded
        | h >= 0  && h < 6   = 0
        | h >= 6  && h < 12  = 6 
        | h >= 12 && h < 18  = 12
        | h >= 18 && h <= 23 = 18
  in fromGregToZoned timeZone mins y mo d rounded 0
     
{-test h m = do
  (zone,mins) <- getZone
  return
    $ roundAt6Hours
    $ utcToZonedTime zone $ fromGregorian 1 1 1 h (m-mins) 0-}
    
intervalConstraint :: [Command] -> IO (Maybe IntervalConstraint)
intervalConstraint = iCons . filter ((== ICons) . commandType)
  where
    iCons  [] = return Nothing
    iCons [x] = Just <$> commandToICons x
    iCons   _ = error "More than one interval constraint defined."

    commandToICons :: Command -> IO IntervalConstraint
    commandToICons c = case c of
      NotinC    spec -> Notin    <$> fromSpecialToNonSpecial spec
      InC       spec -> In       <$> fromSpecialToNonSpecial spec
      NotbeginC spec -> Notbegin <$> fromSpecialToNonSpecial spec
      BeginC    spec -> Begin    <$> fromSpecialToNonSpecial spec
      NotendC   spec -> Notend   <$> fromSpecialToNonSpecial spec
      EndC      spec -> End      <$> fromSpecialToNonSpecial spec

    isMorning = undefined
    
    fromSpecialToNonSpecial ::
      SpecialInterval -> IO [Interval ZonedTime]
    fromSpecialToNonSpecial spec = case spec of
      
      Morning    -> makeIntervals isMorning (Hours 6) (Days 1)
      Afternoon  -> undefined
      Evening    -> undefined
      Night      -> undefined
    
      Sunday     -> undefined
      Monday     -> undefined
      Tuesday    -> undefined
      Wednesday  -> undefined
      Thursday   -> undefined
      Friday     -> undefined
      Saturday   -> undefined

      FirstWeek  -> undefined
      SecondWeek -> undefined
      ThirdWeek  -> undefined
      FourthWeek -> undefined

      January    -> undefined
      February   -> undefined
      March      -> undefined
      April      -> undefined
      May        -> undefined
      June       -> undefined
      July       -> undefined
      August     -> undefined
      September  -> undefined
      October    -> undefined
      November   -> undefined
      December   -> undefined

      Interval i -> undefined

      And s1 s2  -> undefined
      Or  s1 s2  -> undefined

{-nextDateWithConstraint :: (ZonedTime -> Bool) -> Dur -> IO (Head, ZonedTime)
nextDateWithConstraint constr plusTime = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  date <- getCurrentTime
  case constr date of
    True  -> date
    False -> nextDateWithConstraint constr plusTime
             $ nextTime plusTime date
  where nextTime dur date = case dur of
          Hours h' ->
            let (y,mo,d,h,m,s) = D.toGregorian date
            in fromGregorian (y,mo,d,h,m+m',s)-}

---------------------------------
      
place :: [Command] -> Maybe Place
place = place' . filter ((== PlaceT) . commandType)
  where
    place' :: [Command] -> Maybe Place
    place'  [] = Nothing
    place' [x] = Just (commandToPlace x)
    place'   _ = error "More than one place defined."

    commandToPlace :: Command -> Place
    commandToPlace (AtC name) = At name

messg :: [Command] -> Maybe Messg
messg = messg' . filter ((== MsgT) . commandType)
  where
    messg' :: [Command] -> Maybe Messg
    messg'  [] = Nothing
    messg' [x] = Just (commandToMessg x)
    messg'   _ = error "More than one place defined."

    commandToMessg :: Command -> Messg
    commandToMessg (MsgC   msg) = Msg  msg
    commandToMessg (MsgsC msgs) = Msgs msgs

constr :: [Command] -> [Constraint]
constr = map commandToConstr . filter ((== ConsT) . commandType)
  where
    commandToConstr :: Command -> Constraint
    commandToConstr comm = case comm of
      AfterC       name -> After       name
      BeforeC      name -> Before      name
      RightafterC  name -> RightAfter  name
      RightbeforeC name -> RightBefore name

costs :: [Command] -> [Cost]
costs = map commandToCosts . filter ((== CosT) . commandType)
  where
    commandToCosts :: Command -> Cost
    commandToCosts _ = Cst

{-addOn :: Command -> IO AddOn
addOn comm = case commandType comm of
  ICons  -> IntervalConstraint <$> iCons comm
  ConsT  -> Constraint <$> (return $ constr comm)
  PlaceT -> Place      <$> (return $ place  comm)
  MsgT   -> Messg      <$> (return $ messg  comm)
  CosT   -> Cost       <$> (return $ cost   comm)-}

{-iCons :: Command -> IO IntervalConstraint
iCons = undefined

iCons (InitC inst) = Init <$> case inst of
  PutYearAndMonth date -> putYearAndMonth date
  PutYear         date -> putYear date
  PutToday        date -> putToday date
  CompleteTime    date -> return date
iCons (EndC  inst) = End  <$> case inst of
  PutYearAndMonth date -> putYearAndMonth date
  PutYear         date -> putYear date
  PutToday        date -> putToday date
  CompleteTime    date -> return date
--iCons RightaftersleepC  = return Rightbeforesleep
--iCons RightbeforesleepC = return Rightbeforesleep
iCons (NotinC int) = Notin <$> notinProcess int -}

{-constrs :: Command -> Maybe Constraint
constrs comm = case comm of
  AfterC       name -> After       name
  BeforeC      name -> Before      name
  RightafterC  name -> RightAfter  name
  RightbeforeC name -> RightBefore name

place :: Command -> Maybe Place
place (AtC plc) = At plc

messg :: Command -> Maybe Messg
messg comm = case comm of
  MsgC  txt  -> Msg  txt
  MsgsC txts -> Msgs txts

cost :: Command -> Maybe Cost
cost = Cst

notinProcess :: [Interval Instant] -> IO [Interval ZonedTime]
notinProcess = mapM intervalProcess
  where
    intervalProcess :: Interval Instant -> IO (Interval ZonedTime)
    intervalProcess (FromTo from to)
      = FromTo <$> i2z from <*> i2z to
    intervalProcess (OrMore t) = OrMore <$> i2z t
    intervalProcess (Exact  t) = Exact <$> i2z t
    intervalProcess NoInterval = return NoInterval
    i2z :: Instant -> IO ZonedTime
    i2z = instantToZonedTime -}
    
------------------------- Time Functions

fromInstantToCompleteTime :: Instant -> IO ZonedTime
fromInstantToCompleteTime inst = case inst of
  PutYearAndMonth date -> putYearAndMonth date
  PutYear         date -> putYear         date
  PutToday        date -> putToday        date
  CompleteTime    date -> return          date

putYearAndMonth :: ZonedTime -> IO ZonedTime
putYearAndMonth time = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  date <- getCurrentTime
  let (y,mo,_) = toGregorian' date
      (_,_,d,h,m,s) = toGregorian $ zonedTimeToUTC time
  return
    $ utcToZonedTime timeZone
    $ fromGregorian y mo d h (m-mins) s

putYear :: ZonedTime -> IO ZonedTime
putYear time = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  date <- getCurrentTime
  let (y,_,_) = toGregorian' date
      (_,mo,d,h,m,s) = toGregorian $ zonedTimeToUTC time
  return
    $ utcToZonedTime timeZone
    $ fromGregorian y mo d h (m-mins) s

putToday :: ZonedTime -> IO ZonedTime
putToday time = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  date <- getCurrentTime
  let (y,mo,d) = toGregorian' date
      (_,_,_,h,m,s) = toGregorian $ zonedTimeToUTC time
  return
    $ utcToZonedTime timeZone
    $ fromGregorian y mo d h (m-mins) s

today :: IO ZonedTime
today = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  timeUTC <- getCurrentTime
  let (y,m,d) = toGregorian' timeUTC
  return
    $ utcToZonedTime timeZone
    $ fromGregorian y m (d+1) 0 (-mins) 0

durToSeconds :: Dur -> Seconds
durToSeconds dur = case dur of
  Minutes  m -> m*60
  Hours    h -> h*3600
  Days     d -> d*86400
  Weeks    w -> w*604800
  Months  mo -> mo*18144000

{-durToSeconds :: Dur -> Seconds
  Minutes  m -> m*60
  Hours    h -> h*3600
  Days     d -> d*86400
  Weeks    w -> w*604800
  Months  mo -> mo*18144000-}

durToMinutes :: Dur -> Minutes
durToMinutes dur = case dur of
  Minutes  m -> m
  Hours    h -> h*60
  Days     d -> d*1440
  Weeks    w -> w*10080
  Months  mo -> mo*302400

fromInstantToInitialTime :: Instant -> IO ZonedTime
fromInstantToInitialTime inst = case inst of
   PutYear z         -> putYearInitial z
   PutYearAndMonth z -> putYearAndMonthInitial z
   PutToday z        -> putDayInitial z
   CompleteTime z    -> return z

putYearInitial ::  ZonedTime -> IO ZonedTime
putYearInitial z = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  let (_,mo,d,h,m,s) = toGregorian $ zonedTimeToUTC z
  return
    $ utcToZonedTime timeZone
    $ fromGregorian 1 mo d h (m-mins) s

putYearAndMonthInitial ::  ZonedTime -> IO ZonedTime
putYearAndMonthInitial z = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  let (_,_,d,h,m,s) = toGregorian $ zonedTimeToUTC z
  return
    $ utcToZonedTime timeZone
    $ fromGregorian 1 1 d h (m-mins) s

putDayInitial ::  ZonedTime -> IO ZonedTime
putDayInitial z = do
  ZonedTime _ timeZone@(TimeZone mins _ _) <- getZonedTime
  let (_,_,_,h,m,s) = toGregorian $ zonedTimeToUTC z
  return
    $ utcToZonedTime timeZone
    $ fromGregorian 1 1 1 h (m-mins) s

----------------------- Helpers


{----- TODO

Arrumar os tempos para que todos os tempos fiquem certos
de acordo com a zona e nao com o UTC - DONE

Processar os AddOns - DONE

Pesquisar a ideia de dividir os AddOns - DONE

----------}

{-           
makeProject :: [Command] -> IO Project
makeProject (sort -> cs) = case projectType cs of
   CD_    -> do
     as <- addOns (drop 2 cs) 
     return $ CD
       (cycl (cs !! 0))
       (duration (cs !! 1))
       as
   CD'_   -> do
     as <- addOns (drop 2 cs) 
     return $ CD'
       (cycl (cs !! 0))
       (duration' (cs !! 1))
       as
   DE_    -> do
     e <- endpoint (cs !! 1)
     as <- addOns (drop 2 cs) 
     return $ DE
       (duration (cs !! 0))
       e
       as
   CE_    -> do
     e <- endpoint (cs !! 1)
     as <- addOns (drop 2 cs) 
     return $ CE
       (cycl (cs !! 0))
       e
       as
   ET_    -> do
     e <- endpoint (cs !! 0)
     as <- addOns (drop 2 cs) 
     return $ ET
       e
       (totaltime (cs !! 1))
       as
   CT_    -> do
     as <- addOns (drop 2 cs) 
     return $ CT
       (cycl (cs !! 0))
       (totaltime (cs !! 1))
       as
   CDE_   -> do
     e <- endpoint (cs !! 2)
     as <- addOns (drop 3 cs) 
     return $ CDE
       (cycl (cs !! 0))
       (duration (cs !! 1))
       e
       as
   CD'E_  -> do
     e <- endpoint (cs !! 2)
     as <- addOns (drop 3 cs) 
     return $ CD'E
       (cycl (cs !! 0))
       (duration' (cs !! 1))
       e
       as
   CDT_   -> do
     as <- addOns (drop 3 cs)
     return $ CDT
       (cycl (cs !! 0))
       (duration (cs !! 1))
       (totaltime (cs !! 2))
       as
   CD'T_  -> do
     as <- addOns (drop 3 cs) 
     return $ CD'T
       (cycl (cs !! 0))
       (duration' (cs !! 1))
       (totaltime (cs !! 2))
       as
   CET_   -> do
     e <- endpoint (cs !! 1)
     as <- addOns (drop 3 cs) 
     return $ CET
       (cycl (cs !! 0))
       e
       (totaltime (cs !! 2))
       as
   DET_   -> do
     e <- endpoint (cs !! 1)
     as <- addOns (drop 3 cs) 
     return $ DET
       (duration (cs !! 0))
       e
       (totaltime (cs !! 2))
       as
   D'ET_  -> do
     e <- (endpoint (cs !! 1))
     as <- addOns (drop 3 cs) 
     return $ D'ET
       (duration' (cs !! 0))
       e
       (totaltime (cs !! 2))
       as
   CDET_  -> do
     e <- endpoint (cs !! 2)
     as <- addOns (drop 4 cs) 
     return $ CDET
       (cycl (cs !! 0))
       (duration (cs !! 1))
       e
       (totaltime (cs !! 3))
       as
   CD'ET_ -> do
     e <- endpoint (cs !! 2)
     as <- addOns (drop 4 cs) 
     return $ CD'ET
       (cycl (cs !! 0))
       (duration' (cs !! 1))
       e
       (totaltime (cs !! 3))
       as
-}

{-duration' :: Command -> Duration
duration' comm = Duration $ cycle $ case comm of
  DurationsiC tims ->
    map
    (\tim -> (Nothing, [(Nothing, tim, NoInterval)]))
    tims
  DurationsniC nameNtims ->
    map
    (\(name,tim) -> (Just name, [(Nothing, tim, NoInterval)]))
    nameNtims
  DivisionsiC timNintsList ->
    map
    (\timNints ->
      (Nothing,
       map
       (\(tim,int) -> (Nothing,tim,int))
       timNints))
    timNintsList
  DivisionsniC nameNtimNintsList ->
    map
    (\nameNtimNints ->
      (Nothing,
       map
       (\(name,tim,int) -> (Just name,tim,int))
       nameNtimNints))
    nameNtimNintsList
  DivisionsnniC nameNtimNintsList ->
    map
    (\(name,nameNtimNints) ->
      (Just name,
       map
       (\(name',tim,int) -> (Just name',tim,int))
       nameNtimNints))
    nameNtimNintsList
  _ -> error "duration': Wrong command type."-}
