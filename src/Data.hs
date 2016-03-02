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
  | BeforeC Name
  | RightafterC Name
  | RightbeforeC Name

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
    (Maybe (Totaltime Minutes))
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
  = CycleS Int Minutes
  deriving (Show, Read, Eq)

data DurationS
  = DurationS    [(Maybe Name, [(Maybe Name, Dur, Interval Dur)])]
  | DurationInfS [(Maybe Name, [(Maybe Name, Dur, Interval Dur)])]
  deriving (Show,Read,Eq)

data Endpoint
  = Endpoint ZonedTime
  deriving (Show, Read, Eq)

data Totaltime x
  = Totaltime x
  | EffectiveTotaltime x
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
           
{-data IntervalConstraint
  = Notin    [InOrOut (Interval ZonedTime)]
  | In       [InOrOut (Interval ZonedTime)]
  | Notbegin [InOrOut (Interval ZonedTime)]
  | Begin    [InOrOut (Interval ZonedTime)]
  | Notend   [InOrOut (Interval ZonedTime)]
  | End      [InOrOut (Interval ZonedTime)]
  deriving (Show, Read, Eq)-}

data Constraint
  = After Name
  | Before Name
  | RightAfter Name
  | RightBefore Name
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

----------------- State And Project

newtype StepsDone
  = StepsDone Int
  deriving (Show, Read, Eq)
           
newtype TimeSpent
  = TimeSpent Minutes
  deriving (Show, Read, Eq)

data State
  = State (Maybe StepsDone) (Maybe TimeSpent)
  deriving (Show, Read, Eq)

data IsFinished x
  = Finished | Unfinished x
  deriving (Show, Read, Eq)

{-
type State = State [(Name, StateType)]

data StateType = StepsDone Int
               | TimeSpent Minutes
               | Both Int Minutes
               deriving (Show, Read, Eq)
-}

----------------- Project

data TotalProject' =
  TotalProject' Cycle' Duration' Endpoint' Totaltime' AddOns'

---

data TotalProject = TotalProject Cycle' Duration' AddOns'

---

data Endpoint' = Maximum | Endpoint' ZonedTime

data Totaltime' = Infinite | Finite (Totaltime Minutes)

type Message = String

data Cycle' = Cycle' [Interval ZonedTime]

data Duration' =
  Duration'
  [(Maybe Name,
    Maybe Message,
    [(Maybe Name, Maybe Message, Dur, Interval Dur)]
   )]
  deriving (Show,Read,Eq)

newtype AddOns' = AddOns' [AddOn']

data AddOn' = Constraint' Constraint
            | Place' Place
            | Cost' Cost
            deriving (Show, Read, Eq)


{-
data Project =
    CD    Cycle Duration AddOns
  | CD'   Cycle Duration AddOns
  | DE    Duration Endpoint AddOns
  | CE    Cycle Endpoint AddOns
  | ET    Endpoint Totaltime AddOns
  | CT    Cycle Totaltime AddOns
  | CDE   Cycle Duration Endpoint AddOns -- r
  | CD'E  Cycle Duration Endpoint AddOns 
  | CDT   Cycle Duration Totaltime AddOns -- r
  | CD'T  Cycle Duration Totaltime AddOns
  | CET   Cycle Endpoint Totaltime AddOns
  | DET   Duration Endpoint Totaltime AddOns -- r
  | D'ET  Duration Endpoint Totaltime AddOns
  | CDET  Cycle Duration Endpoint Totaltime AddOns -- r
  | CD'ET Cycle Duration Endpoint Totaltime AddOns -- r
  deriving (Show, Read, Eq)
-}
