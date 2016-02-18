module Data where

----------------- Imports

import Text.Parsec
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

instance Eq ZonedTime where
  ZonedTime t (TimeZone _ _ n) ==
    ZonedTime t' (TimeZone _ _ n') =
    t == t' &&  n == n'

-- when time is not sure
data Interval t = FromTo { from :: t, to :: t }
                | OrMore t
                | Exact t
                | NoInterval
                deriving (Show, Read, Eq)

data Dur = Minutes Int
         | Hours Int
         | Days Int
         | Weeks Int
         | Months Int
         | Plus Dur Dur
         deriving (Show, Read, Eq)

data Instant = PutYearAndMonth ZonedTime
             | PutYear ZonedTime
             | PutToday ZonedTime
             | CompleteTime ZonedTime
             deriving (Show, Read, Eq)

{-Morning     5 am to 12 pm (noon)
Early morning    5 to 8 am
Late morning     11 am to 12pm
 
Afternoon     12 pm to 5 pm
Early afternoon   1 to 3pm
Late afternoon    4 to 5pm
 
Evening     5 pm to 9 pm
Early evening   5 to 7 pm

Night         9 pm to 4 am-}

data SpecialInterval =
  
    Morning
  | Afternoon
  | Evening
  | Night
    
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

  | FirstWeek
  | SecondWeek
  | ThirdWeek
  | FourthWeek

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

----------------- Project

data CommandType =
    C      -- CycleType
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

data ProjectType =
    CD
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

data OkErr = Ok | Err String

data Project =
  Project
  (Maybe Cycle)
  (Maybe Duration)
  (Maybe Endpoint)
  (Maybe Totaltime)
  Rest
  deriving (Show, Read, Eq)

data Rest =
  Rest
  (Maybe IntervalConstraint)
  (Maybe Place)
  (Maybe Messg)
  [Constraint]
  [Cost]
  deriving (Show, Read, Eq)
             
type Seconds = Int -- ou Int

--------

data Cycle =
  Cycle Int Seconds
  deriving (Show, Read, Eq)

data Duration =
  Duration [(Maybe Name, [(Maybe Name, Dur, Interval Dur)])]
  deriving (Show,Read,Eq)

data Endpoint =
  Endpoint ZonedTime
  deriving (Show, Read, Eq)

data Totaltime = Totaltime Seconds
               | EffectiveTotaltime Seconds
               deriving (Show, Read, Eq)

newtype AddOns = AddOns [AddOn] deriving (Show, Read, Eq)

data AddOn = IntervalConstraint IntervalConstraint
           | Constraint Constraint
           | Place Place
           | Messg Messg
           | Cost Cost
           deriving (Show, Read, Eq)

data IntervalConstraint = Notin    [Interval ZonedTime]
                        | In       [Interval ZonedTime]
                        | Notbegin [Interval ZonedTime]
                        | Begin    [Interval ZonedTime]
                        | Notend   [Interval ZonedTime]
                        | End      [Interval ZonedTime]
                        deriving (Show, Read, Eq)

data Constraint = After Name
                | Before Name
                | RightAfter Name
                | RightBefore Name
                deriving (Show,Read,Eq)

data Place = At String
           deriving (Show,Read,Eq)

data Messg = Msgs [String]
           | Msg String
           deriving (Show,Read,Eq)

data Cost = Cst -- Fazer mais costs
          deriving (Show,Read,Eq)


----------------- TotalProject

data TotalProject' =
  TotalProject' Cycle' Duration' Endpoint' Totaltime' AddOns'

---

data TotalProject = TotalProject Cycle' Duration' AddOns'

---

data Endpoint' = Maximum | Endpoint' ZonedTime

data Totaltime' = Infinite | Finite Totaltime

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
