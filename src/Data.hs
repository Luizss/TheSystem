{-# language DeriveFunctor #-}

module Data where

----------------- Imports

import Text.Parsec hiding (State)
import Data.Time

----------------- Parser

type File      = String
type Name      = String
type Keyword   = String
type Arguments = String

type Parser a = Parsec String () a

data Parsed =
  Parsed Name [(Keyword, Arguments)]
  deriving (Show,Read,Eq)

----------------- Command

data Interval t
  = FromTo { from :: t, to :: t }
  | OrMore t
  | Exact t
  | NoInterval
  deriving (Show, Read, Eq)

data Dur
  = Minutes Int
  | Hours Int
  | Days Int
  | Weeks Int
  | Months Int
  | Plus Dur Dur
  deriving (Show, Read, Eq)

data Instant
  = PutYearAndMonth ZonedTime
  | PutYear ZonedTime
  | PutToday ZonedTime
  | CompleteTime ZonedTime
  deriving (Show, Read, Eq)

data SpecialInterval
  = Dawn    -- 00 - 05:59
  | Morning -- 06 - 11:59
  | Evening -- 12 - 17:59
  | Night   -- 18 - 23:59
    
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

  | January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

  | Interval (Interval Instant)
  | Complete (Interval ZonedTime)

  | And SpecialInterval SpecialInterval
  | Or SpecialInterval SpecialInterval
  deriving (Show, Eq, Read)
             
data Command  
  = CycleC Int Dur

  | DurationC    Dur
  | DurationsC   [Dur]
  | DurationsiC  [Dur]
  | DurationsnC  [(Name,Dur)]
  | DurationsniC [(Name,Dur)]

  | DivisionC     [(Dur, Interval Dur)]
  | DivisionnC    [(Name, Dur, Interval Dur)]
  | DivisionsC    [[(Dur, Interval Dur)]]
  | DivisionsiC   [[(Dur, Interval Dur)]]
  | DivisionsnC   [[(Name, Dur, Interval Dur)]]
  | DivisionsniC  [[(Name, Dur, Interval Dur)]]
  | DivisionsnnC  [(Name, [(Name, Dur, Interval Dur)])]
  | DivisionsnniC [(Name, [(Name, Dur, Interval Dur)])]

  -- Endpoints
  | TodayC
  | EndpointC Instant

  -- Totaltime
  | TotaltimeC Dur
  | EffectiveTotaltimeC Dur

  -- Interval Constraints
  | NotinC    SpecialInterval
  | InC       SpecialInterval
  | NotbeginC SpecialInterval
  | BeginC    SpecialInterval
  | NotendC   SpecialInterval
  | EndC      SpecialInterval

  -- Constraints
  | AfterC Name
--  | BeforeC Name
  | RightafterC Name
-- | RightbeforeC Name

  -- Place
  | AtC Name

  -- Messages
  | MsgC String
  | MsgsC [String]    
  deriving (Show,Read,Eq)

-- Instances

instance Eq ZonedTime where
  ZonedTime t (TimeZone _ _ n) ==
    ZonedTime t' (TimeZone _ _ n') =
    t == t' && n == n'

instance Functor Interval where
  fmap f int = case int of
    FromTo from to -> FromTo (f from) (f to)
    OrMore t       -> OrMore (f t)
    Exact  t       -> Exact  (f t)
    NoInterval     -> NoInterval

----------------- ProjectS

data OkErr = Ok | Err String

-- Year, Month, Day, Hour, Minute, Second
type Gregorian = (Integer,Int,Int,Int,Int,Int)

type Minutes = Int

data CommandType
  = C      -- CycleType
  | D      -- DurationType
  | D'     -- Infinite DurationType
  | E      -- EndpointType 
  | T      -- TotaltimeType 
  | ICons  -- IntervalConstraints
  | ConsT  -- ConstraintsType 
  | PlaceT -- PlaceType
  | MsgT   -- MessagesType
  | CosT   -- CostsType
  deriving (Show,Read,Ord,Eq)

data ProjectType
  = CD
  | CD'
  | DE
  | CE
  | ET
  | CT
  | CDE
  | CD'E
  | CDT
  | CD'T
  | CET
  | DET
  | D'ET
  | CDET
  | CD'ET
  deriving (Show, Read, Eq)

data ProjectS
  = ProjectS
    (Maybe CycleS)
    (Maybe DurationS)
    (Maybe Endpoint)
    (Maybe Totaltime)
    RestS
  deriving (Show, Read, Eq)

data RestS
  = RestS
    (Maybe IntervalConstraintS)
    (Maybe Place)
    (Maybe Messg)
    [Constraint]
    [Cost]
  deriving (Show, Read, Eq)

data CycleS
  = CycleS Int {-new-} Int Dur
  deriving (Show, Read, Eq)

data DurationS
  = DurationS    [(Maybe Name, [(Maybe Name, Dur, Interval Dur)])]
  | DurationInfS [(Maybe Name, [(Maybe Name, Dur, Interval Dur)])]
  deriving (Show,Read,Eq)

data Endpoint
  = Endpoint ZonedTime
  deriving (Show, Read, Eq)

data Totaltime
  = Totaltime Minutes
  | EffectiveTotaltime Minutes
  | Infinity
  deriving (Show, Read, Eq)

type InOrOut a = (Bool,a)

data IntervalConstraintS
  = NotinS    SpecialInterval
  | InS       SpecialInterval
  | NotbeginS SpecialInterval
  | BeginS    SpecialInterval
  | NotendS   SpecialInterval
  | EndS      SpecialInterval
  deriving (Show, Read, Eq)

data Constraint
  = RightAfter Name
  | After Name
--  | Before Name
  | AfterID Id --
--  | BeforeID Id --
--  | RightBefore Name
  deriving (Show,Read,Eq)

data Place
  = At String
  deriving (Show,Read,Eq)

data Messg
  = Msgs [String]
  | Msg String
  deriving (Show,Read,Eq)

data Cost
  = Cst -- Fazer mais costs
  deriving (Show,Read,Eq)

instance Ord ZonedTime where
  ZonedTime time _ <= ZonedTime time' _
    = time <= time'

----------------- State And Infer

newtype DurationSteps
  = StepsDone Int
  deriving (Show, Read, Eq)

newtype CycleSteps
  = StepsLeft Int
  deriving (Show, Read, Eq)

newtype TimeSpent
  = TimeSpent Minutes
  deriving (Show, Read, Eq)

data State
  = State
    (Maybe CycleSteps)
    (Maybe DurationSteps)
    (Maybe TimeSpent)
  deriving (Show, Read, Eq)

data IsFinished x
  = Finished | Unfinished x
  deriving (Show, Read, Eq)

----------------- Project

type Message = String

data DoubleLinked a = DL [a] a [a] deriving Show

data Project
  = Project CycleP DurationP RestP
  deriving (Show,Eq)

data CycleP
  = CycleP BIEn [(Int, [InOrOut (Interval ZonedTime)])]
  deriving Eq

instance Show CycleP where
  show (CycleP bien s)
    = "CycleP "
      ++ show bien
      ++ " "
      ++ show (take 20 s)

data DurationP
  = DurationP
    [( Maybe Name, Maybe Message
     , [(Maybe Name, Dur, Interval Dur)])]
  deriving Eq

instance Show DurationP where
  show (DurationP lst)
    = "DurationP " ++ show (tos lst)
  -- = "k = " ++ show (length lst)

tos = map (\(_,_,l) -> map (\(_,d,_) -> d) l)

data BIEn
  = B | I | En
  deriving (Show,Eq,Read)
--- begin | in | end

data RestP
  = RestP
    { getPlace  :: (Maybe Place)
    , getConstr :: [Constraint]
    , getCost   :: [Cost] }
  deriving (Show, Read, Eq)

data IntervalConstraint
  = Notin    [InOrOut (Interval ZonedTime)]
  | In       [InOrOut (Interval ZonedTime)]
  | Notbegin [InOrOut (Interval ZonedTime)]
  | Begin    [InOrOut (Interval ZonedTime)]
  | Notend   [InOrOut (Interval ZonedTime)]
  | End      [InOrOut (Interval ZonedTime)]
  deriving (Show, Read, Eq)

----------------- Activities

type DurH
  = (Maybe Name, Maybe Message
    , [(Maybe Name, Dur, Interval Dur)])
type DursH       = [DurH]
type MaxDuration = Minutes
type OkInterval  = [InOrOut (Interval ZonedTime)]
data EndEvent    = EndEvent deriving (Show,Eq,Read)

data Id
  = NoIdYet | Id Int
  deriving (Read, Eq, Ord, Show)

data Activity
  = Activity {
      getName :: Name
    , getId :: Id
    , getMaxDuration :: MaxDuration
    , getOkInterval :: OkInterval
    , getMaybePlace :: (Maybe Place)
    , getMaybeEndEvent :: (Maybe EndEvent)
    , getConstraints :: [Constraint]
    , getCosts :: [Cost]
    , getMessage :: Message
    , getMaybeDiv :: (Maybe (Interval Dur, Activity))
    } deriving (Show,Eq,Read)

----------------- Schedule

type Schedule = [Slot]

type OkSchedule = [(Bool, Slot)]

data Slot
  = Slot
    (Maybe ActivityPrim)
    (Interval ZonedTime) -- fromto
  deriving (Show, Read, Eq)

data ActivityPrim
  = ActivityPrim
    Name
    Id
    Message
    MaxDuration
    [Constraint]
    (Maybe Place)
    (Maybe EndEvent)
  deriving (Show, Read, Eq)

data Tree a
  = Tree a [Tree a]
    deriving (Show, Read, Eq, Functor)

--data WinFail a = Fail a | Win a

----------------- Total Project

data TotalProject' =
  TotalProject' Cycle' Duration' Endpoint' Totaltime' AddOns'

---

data TotalProject = TotalProject Cycle' Duration' AddOns'

---

data Endpoint' = Maximum | Endpoint' ZonedTime

data Totaltime' = Infinite | Finite Totaltime

data Cycle' = Cycle' [Interval ZonedTime]

data Duration' =
  Duration'
  [( Maybe Name
   , Maybe Message
   , [(Maybe Name, Maybe Message, Dur, Interval Dur)])]
  deriving (Show,Read,Eq)

newtype AddOns' = AddOns' [AddOn']

data AddOn' = Constraint' Constraint
            | Place' Place
            | Cost' Cost
            deriving (Show, Read, Eq)
