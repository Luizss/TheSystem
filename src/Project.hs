{-# language TupleSections, LambdaCase #-}

module Project where

-------------------- Imports

import Prelude hiding (truncate)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.DateTime
import Data.Time.Calendar.WeekDate

import Data
import ProjectS
import StateAndInfer

-------------------- Transformations

transforms
  :: [(Name, (CycleS, DurationS, Totaltime, Maybe Endpoint, RestS))]
  -> IO [(Name, Project)]
transforms = mapM (\(n,t) -> (n,) <$> transform t)

transform
  :: (CycleS, DurationS, Totaltime, Maybe Endpoint, RestS)
  -> IO Project
transform p@(c,d,_,me,r) = do
  d' <- truncate p
  c' <- mergeCycleAndIntervalConstraint c r
  let d'' = mergeDurationAndMsg d' r
      r'  = newRest r
  return $ Project c' d'' r'
  where
    newRest :: RestS -> RestP
    newRest (RestS _ place _ constrs costs)
      = RestP place constrs costs

left :: DoubleLinked a -> DoubleLinked a
left same@(DL left x right) = case left of
  []   -> same
  l:ls -> DL ls l (x:right)

right :: DoubleLinked a -> DoubleLinked a
right same@(DL left x right) = case right of
  []   -> same
  r:rs -> DL (x:left) r rs

leftForever :: DoubleLinked a -> DoubleLinked a
leftForever dl@(DL [] _ _) = dl
leftForever dl = leftForever (left dl)

toDoubleLinked :: [a] -> DoubleLinked a
toDoubleLinked (l:lst) = DL [] l lst
toDoubleLinked _ = error "Nope"

fromDoubleLinked :: DoubleLinked a -> [a]
fromDoubleLinked (DL left middle right)
  = reverse left ++ [middle] ++ right

projectsInOrder :: [(Name,Project)] -> [(Name, Project)]
projectsInOrder [] = []
projectsInOrder pr
  = fromDoubleLinked $ inOrder' $ toDoubleLinked $ pr
  where
    inOrder'
      :: DoubleLinked (Name, Project)
      -> DoubleLinked (Name, Project)
    inOrder' = inOrder 30

    inOrder
      :: Int
      -> DoubleLinked (Name, Project)
      -> DoubleLinked (Name, Project)
    inOrder _ dl@(DL _ _ []) = dl
    inOrder 0 _ = error "After constraints problem :/"
    inOrder n dl@(DL before npr after)
      = checkConstraints (constr npr)
      where

        constr (_ , Project _ _ (RestP _ c _)) = c

        checkConstraints
          :: [Constraint]
          -> DoubleLinked (Name, Project)
        checkConstraints [] = inOrder (n-1) (right dl)
        checkConstraints (c:cs) = case c of
          RightAfter na -> cases na
                           (isRightBefore na before)
                           (isThere na after)
          After      na -> cases na
                           (isThere na before)
                           (isThere na after)
          AfterID     _ -> checkConstraints cs
          where 
            cases _ True _ = checkConstraints cs
            cases na False True =
              let (after', p) = takeProjectOut na after
                  before' = putProjectIn p before
              in inOrder
                 (n-1)
                 (leftForever $ DL before' npr after')
            cases _ False False = checkConstraints cs

        isRightBefore :: Name -> [(Name, Project)] -> Bool
        isRightBefore name ((name', _) : _) = name == name'
        isRightBefore _ _ = False

        isThere :: Name -> [(Name, Project)] -> Bool
        isThere name = isNotEmpty . filter ((==name) . fst)
          where isNotEmpty [] = False
                isNotEmpty _  = True

        takeProjectOut
          :: Name
          -> [(Name,Project)]
          -> ([(Name,Project)], (Name, Project))
        takeProjectOut name xs
          = ( filter ((/=name) . fst) xs
            , check (filter ((==name) . fst) xs))
          where
            check = \case
              []  -> error "?????0??????"
              [x] -> x
              _   -> error "??????2???"

        putProjectIn
          :: (Name,Project)
          -> [(Name, Project)]
          -> [(Name, Project)]
        putProjectIn p ps = ps ++ [p]

-------------------- Duration And Message Merging

mergeDurationAndMsg
  :: DurationS -> RestS -> DurationP
mergeDurationAndMsg (DurationS d) (RestS _ _ mm _ _) = case mm of
  Nothing ->
    DurationP $ map (\(a,b) -> (a, Nothing, b)) d

  Just msg ->
    DurationP $ case msg of
      Msg  txt  -> map (\(a,b) -> (a, Just txt, b)) d
      Msgs txts
        | length txts >= length d
          -> zipWith (\(a,b) txt -> (a, Just txt, b)) d txts
        | otherwise
          -> zipWith (\(a,b) txt -> (a, Just txt, b)) d txts
             ++ drop
             (length txts)
             (map (\(a,b) -> (a,Nothing,b)) d)

-------------------- Cycle And Interval Constraints Merging
      
mergeCycleAndIntervalConstraint
  :: CycleS -> RestS -> IO CycleP
mergeCycleAndIntervalConstraint c (RestS ic _ _ _ _) = case ic of
  Nothing     -> do
    ci <- cycleToListOfIntervals c
    return $ CycleP B (map stripSomeThings ci) -- B?
  Just iConsS -> do
    mi <- makeIntervalConstraint iConsS
    ci <- cycleToListOfIntervals c
    return $ mergeBoth ci mi

cycleToListOfIntervals
  :: CycleS -> IO [(Int, [InOrOut (Interval ZonedTime)])]
cycleToListOfIntervals (CycleS times firstTime dur) = case dur of
  Minutes m -> putTimes <$> makeCycleList nextMinuteSeed
  Hours   h -> putTimes <$> makeCycleList nextHourSeed
  Days    s -> putTimes <$> makeCycleList nextDaySeed
  Weeks   w -> putTimes <$> makeCycleList nextWeekSeed
  Months mo -> putTimes <$> makeCycleList nextMonthSeed
  plus -> cycleToListOfIntervals
          $ CycleS times firstTime
          $ Minutes
          $ durToMinutes plus
  where
    durMins = durToMinutes dur

    makeCycleList
      :: (ZonedTime -> ZonedTime)
      -> IO [[InOrOut (Interval ZonedTime)]]
    makeCycleList nextSeed = do
      now <- getZonedTime
      let seed = nextSeed now
          head = FromTo now seed
          rest = makeRestFromSeed seed
      return $ [(True, head)] : rest

    makeRestFromSeed
      :: ZonedTime -> [[InOrOut (Interval ZonedTime)]]
    makeRestFromSeed z =
      let nextMark = roundIfMonth $ addMinutesZoned durMins z
          roundIfMonth = case dur of
            Months mo -> roundAtMonth
            _         -> id
      in [(True, FromTo z nextMark)] : makeRestFromSeed nextMark

    addMinutesZoned :: Minutes -> ZonedTime -> ZonedTime
    addMinutesZoned x z =
      let (y,mo,d,h,m,s) = fromZonedToGreg z
      in fromGregToZoned (getZone z) (y,mo,d,h,m+x,s)

    putTimes
      :: [[InOrOut (Interval ZonedTime)]]
      -> [(Int, [InOrOut (Interval ZonedTime)])]
    putTimes     [] = []
    putTimes (x:xs) = (firstTime, x) : map (\a -> (times,a)) xs

mergeBoth
  :: [(Int, [InOrOut (Interval ZonedTime)])]
  -> IntervalConstraint --[InOrOut (Interval ZonedTime)]
  -> CycleP             --[(Int, [InOrOut (Interval ZonedTime)])]
mergeBoth cycleList intervalConstrList =
  let (isNot,bien,ints) = case intervalConstrList of
        Notin    x -> (True  ,  I, x)
        In       x -> (False ,  I, x)
        Notbegin x -> (True  ,  B, x)
        Begin    x -> (False ,  B, x)
        Notend   x -> (True  , En, x)
        End      x -> (False , En, x)
      ints' = map (notIf isNot) ints
      notIf False x = x
      notIf True (b,int) = (not b, int)
  in CycleP bien $ map stripSomeThings $ mergeBoth' cycleList ints'
  where 
    mergeBoth'
      :: [(Int, [InOrOut (Interval ZonedTime)])]
      -> [InOrOut (Interval ZonedTime)]
      -> [(Int, [InOrOut (Interval ZonedTime)])]
    mergeBoth' ((times,int) : xs) lst
      = (times, mergeIntervals (&&) int lst)
        : mergeBoth' xs lst

--- funçao para retirar alguns intervalos inuteis
--- tudo fica quebrado por causa dessa funçao
--- ou tudo funciona por causa dessa funçao?
stripSomeThings
    :: (Int,[InOrOut (Interval ZonedTime)])
    -> (Int,[InOrOut (Interval ZonedTime)])
stripSomeThings = s2 . s1 where
  s1 id@(int, (b1,FromTo i j) : (b2, FromTo k l) : xs)
    | (i == j) && (j == k) = (int, (b2, FromTo k l) : xs)
    | otherwise            = id
  s1 x = x
  s2 (i, x) = (i, reverse $ s2' $ reverse x)
  s2' id @ ((b1,FromTo i j) : (b2, FromTo k l) : xs)
    | (i == j) && (j == l) = (b2, FromTo k l) : xs
    | otherwise            = id
  s2' x = x

-------------------- Interval Functions

makeIntervalConstraint
  :: IntervalConstraintS -> IO IntervalConstraint
makeIntervalConstraint intS = case intS of
  NotinS    spec -> Notin    <$> fromSpecialToNonSpecial spec
  InS       spec -> In       <$> fromSpecialToNonSpecial spec
  NotbeginS spec -> Notbegin <$> fromSpecialToNonSpecial spec
  BeginS    spec -> Begin    <$> fromSpecialToNonSpecial spec
  NotendS   spec -> Notend   <$> fromSpecialToNonSpecial spec
  EndS      spec -> End      <$> fromSpecialToNonSpecial spec
  where 

    fromSpecialToNonSpecial ::
      SpecialInterval -> IO [InOrOut (Interval ZonedTime)]
    fromSpecialToNonSpecial spec = case spec of

      Dawn       -> makeIntervals isDawn      (Hours 6) (Days 1)
      Morning    -> makeIntervals isMorning   (Hours 6) (Days 1)
      Evening    -> makeIntervals isEvening   (Hours 6) (Days 1)
      Night      -> makeIntervals isNight     (Hours 6) (Days 1)
    
      Sunday     -> makeIntervals isSunday    (Days 1) (Weeks 1)
      Monday     -> makeIntervals isMonday    (Days 1) (Weeks 1)
      Tuesday    -> makeIntervals isTuesday   (Days 1) (Weeks 1)
      Wednesday  -> makeIntervals isWednesday (Days 1) (Weeks 1)
      Thursday   -> makeIntervals isThursday  (Days 1) (Weeks 1)
      Friday     -> makeIntervals isFriday    (Days 1) (Weeks 1)
      Saturday   -> makeIntervals isSaturday  (Days 1) (Weeks 1)

      January    -> makeIntervals isJanuary   (Months 1) (Months 12)
      February   -> makeIntervals isFebruary  (Months 1) (Months 12)
      March      -> makeIntervals isMarch     (Months 1) (Months 12)
      April      -> makeIntervals isApril     (Months 1) (Months 12)
      May        -> makeIntervals isMay       (Months 1) (Months 12)
      June       -> makeIntervals isJune      (Months 1) (Months 12)
      July       -> makeIntervals isJuly      (Months 1) (Months 12)
      August     -> makeIntervals isAugust    (Months 1) (Months 12)
      September  -> makeIntervals isSeptember (Months 1) (Months 12)
      October    -> makeIntervals isOctober   (Months 1) (Months 12)
      November   -> makeIntervals isNovember  (Months 1) (Months 12)
      December   -> makeIntervals isDecember  (Months 1) (Months 12)

      Interval i -> do
        now   <- getZonedTime
        zTime <- propag $ fmap fromInstantToCompleteTime i
        let FromTo fr to = onlyFromTo zTime
        return
          [(False, FromTo now fr)
          ,(True , FromTo fr to)
          ,(False, FromTo to (add1Year to))]

      Complete zTime -> do
        now <- getZonedTime
        let FromTo fr to = onlyFromTo zTime
        return
          [(False, FromTo now fr)
          ,(True , FromTo fr to)
          ,(False, FromTo to (add1Year to))]

      And s1 s2  ->
        mergeIntervals (&&)
        <$> fromSpecialToNonSpecial s1
        <*> fromSpecialToNonSpecial s2
      Or  s1 s2  ->
        mergeIntervals (||)
        <$> fromSpecialToNonSpecial s1
        <*> fromSpecialToNonSpecial s2

--      x -> error $ show x

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
      
    onlyFromTo (FromTo t r) = FromTo t r
    onlyFromTo (Exact    t) = FromTo t t
    onlyFromTo (OrMore   t) = FromTo t (add1Year t)
    onlyFromTo NoInterval   = NoInterval

    add1Year = addTime (durToMinutes (Months 13))

{- Entendimento dessa funçao exige uma certa teoria
   Ela pega o momento atual e retorna uma lista de intervalos
   em que a funçao 'isIn' e verdadeira ou nao -}
makeIntervals
  :: (ZonedTime -> Bool)
  -> Dur -- Duration of Intervals
  -> Dur -- Period between Intervals
  -> IO [InOrOut (Interval ZonedTime)]
makeIntervals isIn dur per = do
  now <- getZonedTime
  let (headIn,nextMark) = getHeadIn now
      headOut = getHeadOut nextMark
      (_, FromTo a seed) = headOut
      rest = makeRestFromSeed seed --take 10 $ makeRestFromSeed seed
  return $ headIn ++ [headOut] ++ rest
  where
    durMin = durToMinutes dur
    perMin = durToMinutes per
    
    getHeadIn :: ZonedTime
              -> ([InOrOut (Interval ZonedTime)], ZonedTime)
    getHeadIn now = case isIn now of
        False -> ([], now)
        True  -> go now (addTime durMin now)
      where 
        go now nextTime = case isIn nextTime of
          False ->
            let rounded = roundAtBegginingOfInterval nextTime
            in ([(True, FromTo now rounded)], rounded)
          True  -> go now (addTime durMin nextTime)

    getHeadOut :: ZonedTime -> InOrOut (Interval ZonedTime)
    getHeadOut mark = go mark where
      go time =
        let next = addTime durMin time
        in case isIn next of
          True  ->
            (False, FromTo mark (roundAtBegginingOfInterval next))
          False -> go next
      
    roundAtBegginingOfInterval :: ZonedTime -> ZonedTime
    roundAtBegginingOfInterval = case dur of   
      Hours  6 -> roundAt6Hours
      Days   1 -> roundAtDay 
      Weeks  1 -> roundAtWeek
      Months 1 -> roundAtMonth

    makeRestFromSeed :: ZonedTime -> [InOrOut (Interval ZonedTime)]
    makeRestFromSeed seed =
      (True, FromTo seed to)
      : (False, FromTo to newSeed)
      : makeRestFromSeed newSeed
      where
        to = roundIfMonth $ addTime durMin seed
        newSeed = roundIfMonth $ addTime perMin seed
        roundIfMonth = case per of
          Months mo -> roundAtMonth
          _         -> id

mergeIntervals
  :: (Bool -> Bool -> Bool)
  -> [InOrOut (Interval ZonedTime)]
  -> [InOrOut (Interval ZonedTime)]
  -> [InOrOut (Interval ZonedTime)]
mergeIntervals merge x = zipIntervals . go x where
  go ((a, FromTo frA toA) : restA) ((b, FromTo frB toB) : restB)
    | frA == frB && toA < toB =
      (merge a b, FromTo frA toA)
      : go restA ((b, FromTo toA toB) : restB)
    | frA == frB && toA > toB =
        (merge a b, FromTo frA toB)
        : go ((a, FromTo toB toA) : restA) restB
    | frA == frB && toA == toB =
      (merge a b, FromTo frA toA)
      : go restA restB
    | toA < frB =
      --(merge a (not b), FromTo frA toA) :
      go restA ((b, FromTo frB toB) : restB)
    | toB < frA =
        --(merge (not a) b, FromTo frB toB) :
        go ((a, FromTo frA toA) : restA) restB
    | frA < frB =
      --(merge a (not b), FromTo frA frB) :
      go
      ((a, FromTo frB toA) : restA)
      ((b, FromTo frB toB) : restB)
    | frA > frB =
        --(merge (not a) b, FromTo frB frA) :
        go
        ((a, FromTo frA toA) : restA)
        ((b, FromTo frA toB) : restB)
  go [] _ = []
  go _ [] = []

  zipIntervals
    :: [InOrOut (Interval ZonedTime)]
    -> [InOrOut (Interval ZonedTime)]
  zipIntervals  [] = []
  zipIntervals [x] = [x]
  zipIntervals ((a, FromTo x y) : (b, FromTo z w) : xs)
    | y == z && a == b = zipIntervals ((a, FromTo x w) : xs)
    | otherwise = (a, FromTo x y)
                  : zipIntervals ((b, FromTo z w) : xs)

addTime :: Minutes -> ZonedTime -> ZonedTime
addTime amount date@(ZonedTime _ timeZone) =
  utcToZonedTime timeZone
  $ addMinutes (fromIntegral amount)
  $ zonedTimeToUTC date

isPeriodOfDay :: (Int -> Bool) -> ZonedTime -> Bool
isPeriodOfDay f time =
  let (_,_,_,h,_,_) = fromZonedToGreg time in f h

isDayOfWeek :: (Int -> Bool) -> ZonedTime -> Bool
isDayOfWeek f time =
  let (_,_,wd) = fromZonedToWeekDate time in f wd

isMonth :: (Int -> Bool) -> ZonedTime -> Bool
isMonth f time =
  let (_,mo,_,_,_,_) = fromZonedToGreg time in f mo

isDawn      = isPeriodOfDay (\h -> h >= 0  && h <  6)
isMorning   = isPeriodOfDay (\h -> h >= 6  && h < 12)
isEvening   = isPeriodOfDay (\h -> h >= 12 && h < 18)
isNight     = isPeriodOfDay (\h -> h >= 18 && h <= 23)

isSunday    = isDayOfWeek (==7)
isMonday    = isDayOfWeek (==1)
isTuesday   = isDayOfWeek (==2)
isWednesday = isDayOfWeek (==3)
isThursday  = isDayOfWeek (==4)
isFriday    = isDayOfWeek (==5)
isSaturday  = isDayOfWeek (==6)

isJanuary   = isMonth (==1)
isFebruary  = isMonth (==2)
isMarch     = isMonth (==3)
isApril     = isMonth (==4)
isMay       = isMonth (==5)
isJune      = isMonth (==6)
isJuly      = isMonth (==7)
isAugust    = isMonth (==8)
isSeptember = isMonth (==9)
isOctober   = isMonth (==10)
isNovember  = isMonth (==11)
isDecember  = isMonth (==12)

------------------- Rounding Functions

roundAt6Hours :: ZonedTime -> ZonedTime
roundAt6Hours time =
  let zone = getZone time
      (y,mo,d,h,_,_) = fromZonedToGreg time
      rounded
        | h >= 0  && h < 6   = 0
        | h >= 6  && h < 12  = 6 
        | h >= 12 && h < 18  = 12
        | h >= 18 && h <= 23 = 18
  in fromGregToZoned zone (y,mo,d,rounded,0,0)

roundAtDay :: ZonedTime -> ZonedTime
roundAtDay time =
  let zone = getZone time
      (y,mo,d,_,_,_) = fromZonedToGreg time
  in fromGregToZoned zone (y,mo,d,0,0,0)

roundAtWeek :: ZonedTime -> ZonedTime
roundAtWeek time =
  let zone = getZone time
      (y,w,d) = fromZonedToWeekDate time
  in fromWeekDateToZoned zone (y,w,1)

roundAtMonth :: ZonedTime -> ZonedTime
roundAtMonth time =
  let zone = getZone time
      (y,mo,_,_,_,_) = fromZonedToGreg time
  in fromGregToZoned zone (y,mo,1,0,0,0)

nextTemplate
  :: (Gregorian -> Gregorian) -> ZonedTime -> ZonedTime
nextTemplate f z =
  fromGregToZoned (getZone z) $ f $ fromZonedToGreg z
      
nextMinuteSeed
  = nextTemplate (\(y,mo,d,h,m,_) -> (y,mo,d,h,m+1,0))
nextHourSeed
  = nextTemplate (\(y,mo,d,h,_,_) -> (y,mo,d,h+1,0,0))
nextDaySeed
  = nextTemplate (\(y,mo,d,_,_,_) -> (y,mo,d+1,0,0,0))
nextWeekSeed z
  = fromWeekDateToZoned (getZone z)
    $ (\(y,wk,_) -> (y,wk+1,0))
    $ fromZonedToWeekDate z
nextMonthSeed
  = nextTemplate (\(y,mo,_,_,_,_) -> (y,mo+1,1,0,0,0))

-------------------- Testes

u = undefined

bla = do
  CycleP bien lst <-
    mergeCycleAndIntervalConstraint
    (CycleS 2 1 (Weeks 2))
    (RestS Nothing u u u u)
--    (RestS (Just (NotinS (Or Sunday Thursday))) u u u u)
  return $ CycleP bien (take 20 lst)
