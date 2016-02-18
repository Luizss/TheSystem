module Command where

-------------------- Imports

import Prelude hiding (cycle, init)
import Data
import Parser
import Text.Parsec
import Data.Time

-------------------- Recognition of Commands

makeCommand :: (String, String) -> Command
makeCommand (keyw,args) = case keyw of

  "cycle" -> let (nt,time) = cycle args
             in CycleC nt time

  "dur"    -> DurationC    $ duration    args
  "durs"   -> DurationsC   $ durations   args
  "dursi"  -> DurationsiC  $ durationsi  args
  "dursn"  -> DurationsnC  $ durationsn  args
  "dursni" -> DurationsniC $ durationsni args

  "duration"    -> DurationC    $ duration    args
  "durations"   -> DurationsC   $ durations   args
  "durationsi"  -> DurationsiC  $ durationsi  args
  "durationsn"  -> DurationsnC  $ durationsn  args
  "durationsni" -> DurationsniC $ durationsni args

  "div"     -> DivisionC     $ division     args
  "divn"    -> DivisionnC    $ divisionn    args
  "divs"    -> DivisionsC    $ divisions    args
  "divsi"   -> DivisionsiC   $ divisionsi   args
  "divsn"   -> DivisionsnC   $ divisionsn   args
  "divsni"  -> DivisionsniC  $ divisionsni  args
  "divsnn"  -> DivisionsnnC  $ divisionsnn  args
  "divsnni" -> DivisionsnniC $ divisionsnni args

  "division"     -> DivisionC     $ division     args
  "divisionn"    -> DivisionnC    $ divisionn    args
  "divisions"    -> DivisionsC    $ divisions    args
  "divisionsi"   -> DivisionsiC   $ divisionsi   args
  "divisionsn"   -> DivisionsnC   $ divisionsn   args
  "divisionsni"  -> DivisionsniC  $ divisionsni  args
  "divisionsnn"  -> DivisionsnnC  $ divisionsnn  args
  "divisionsnni" -> DivisionsnniC $ divisionsnni args

  "today"    -> today args
  "endpoint" -> EndpointC $ endpoint args

  "totaltime" ->
    TotaltimeC $ totaltime args
  "effectivetotaltime" ->
    EffectiveTotaltimeC $ effectivetotaltime args

  "notin"    -> NotinC    $ notin args
  "in"       -> InC       $ notin args
  "notbegin" -> NotbeginC $ notin args
  "begin"    -> BeginC    $ notin args
  "notend"   -> NotendC   $ notin args
  "end"      -> EndC      $ notin args

  "after"       -> AfterC       $ after       args
  "before"      -> BeforeC      $ before      args
  "rightafter"  -> RightafterC  $ rightafter  args
  "rightbefore" -> RightbeforeC $ rightbefore args

  "at"   -> AtC   $ at   args
  "msg"  -> MsgC  $ msg  args
  "msgs" -> MsgsC $ msgs args
  
  txt -> error $ txt ++ " is not a valid command."

recognizeCommand :: Parsed -> (Name, [Command])
recognizeCommand (Parsed name keysAndArgs) =
  (name, map makeCommand keysAndArgs)

recognizeCommands :: [Parsed] -> [(Name, [Command])]
recognizeCommands = map recognizeCommand

-------------------- Argument Handlers

cycle              = parseText cycleParser

duration           = parseText durParser
durations          = parseText dursParser
durationsi         = parseText dursParser
durationsn         = parseText dursnParser
durationsni        = parseText dursnParser

division           = parseText divisionParser
divisionn          = parseText divisionnParser
divisions          = parseText divisionsParser
divisionsi         = parseText divisionsParser
divisionsn         = parseText divisionsnParser
divisionsni        = parseText divisionsnParser
divisionsnn        = parseText divisionsnnParser
divisionsnni       = parseText divisionsnnParser

today              = withArgsMeansError TodayC
endpoint           = parseText instantParserP

totaltime          = parseText durParser
effectivetotaltime = parseText durParser

notin              = parseText exprParser
in'                = parseText exprParser
notbegin           = parseText exprParser
begin              = parseText exprParser
notend             = parseText exprParser
end                = parseText exprParser

after              = checkEmptyString "after" . noSpaces
before             = checkEmptyString "before" . noSpaces
rightafter         = checkEmptyString "rightafter" . noSpaces
rightbefore        = checkEmptyString "rightbefore" . noSpaces

at                 = checkEmptyString "at" . noSpaces

msg                = substForNewLine .
                     checkEmptyString "msg"
                     . noSpaces
msgs               = parseText msgsParser

------------------- Parsers for Commands

cycleParser :: Parser (Int, Dur)
cycleParser = do
  num <- many1 digit
  spaces
  string "in"
  spaces
  dur <- durParser
  return (read num, dur)

durParser :: Parser Dur
durParser = do
  dur <- choice [mons', mins', hours', days', weeks',
                 mons_, mins_, hours_, days_, weeks_,
                 mons,  mins,  hours,  days,  weeks]
  (do char ':'
      dur' <- durParser
      return (Plus dur dur')) <|> return dur

  where
    
    numberAndUnit :: String -> Parser Int
    numberAndUnit s = read <$> do
      num <- many digit
      spaces
      string s
      case num of
        "" -> error "No number given."
        _  -> return num
           
    mons'  = Months  <$> try (numberAndUnit "months")
    mins'  = Minutes <$> try (numberAndUnit "mins")
    hours' = Hours   <$> try (numberAndUnit "hours")
    days'  = Days    <$> try (numberAndUnit "days")
    weeks' = Weeks   <$> try (numberAndUnit "weeks")
    mons_  = Months  <$> try (numberAndUnit "month")
    mins_  = Minutes <$> try (numberAndUnit "min")
    hours_ = Hours   <$> try (numberAndUnit "hour")
    days_  = Days    <$> try (numberAndUnit "day")
    weeks_ = Weeks   <$> try (numberAndUnit "week")
    mons   = Months  <$> try (numberAndUnit "mo")
    mins   = Minutes <$> try (numberAndUnit "m")
    hours  = Hours   <$> try (numberAndUnit "h")
    days   = Days    <$> try (numberAndUnit "d")
    weeks  = Weeks   <$> try (numberAndUnit "w")

dursParser :: Parser [Dur]
dursParser =
  sepBy
  (do dur <- durParser
      spaces
      return dur)
  (char ',' >> spaces)

dursnParser :: Parser [(String, Dur)]
dursnParser = map (ifNoNameError "dursn") <$>
  sepBy          
  (do name <- many (noneOf ":,")
      char ':'
      spaces
      dur <- durParser
      spaces
      return (noSpaces name,dur))
  (char ',' >> spaces)

divisionParser :: Parser [(Dur, Interval Dur)]
divisionParser =
  sepBy
  (do dur <- durParser
      spaces
      char ','
      spaces
      int <- intervalDurParser
      spaces
      return (dur, int))
  (char ',' >> spaces)

divisionnParser :: Parser [(Name, Dur, Interval Dur)]
divisionnParser = map (ifNoNameError2 "divisionn") <$>
  sepBy
  (do name <- many (noneOf ":,")
      char ':'
      spaces
      dur <- durParser
      spaces
      char ','
      spaces
      int <- intervalDurParser
      spaces
      return (noSpaces name, dur, int))
  (char ',' >> spaces)

divisionsParser :: Parser [[(Dur, Interval Dur)]]
divisionsParser =
  sepBy
  (do div <- divisionParser
      spaces
      return div)
  (char ';' >> spaces)

divisionsnParser :: Parser [[(String, Dur, Interval Dur)]]
divisionsnParser =
  sepBy
  (do div <- divisionnParser
      spaces
      return div)
  (char ';' >> spaces)

divisionsnnParser ::
  Parser [(String, [(String, Dur, Interval Dur)])]
divisionsnnParser = map (ifNoNameError "divisionsnn") <$>
  sepBy
  (do nameStep <- many (noneOf ":,")
      char ':'
      spaces
      ds <- divisionnParser
      spaces
      return (noSpaces nameStep,ds))
  (char ';' >> spaces)

exprParser :: Parser SpecialInterval
exprParser = do
  t1 <- term
  spaces
  (try (do string "or"
           spaces
           t2 <- exprParser
           return $ Or t1 t2) <|>
   try (do string "and"
           spaces
           t2 <- exprParser
           return $ And t1 t2) <|>
   (return t1))
  where
    term :: Parser SpecialInterval
    term =
      choice
      [ try $ string "morning"    >> return Morning
      , try $ string "afternoon"  >> return Afternoon
      , try $ string "evening"    >> return Evening
      , try $ string "night"      >> return Night
        
      , try $ string "sunday"     >> return Sunday
      , try $ string "monday"     >> return Monday
      , try $ string "tuesday"    >> return Tuesday
      , try $ string "wednesday"  >> return Wednesday
      , try $ string "thursday"   >> return Thursday
      , try $ string "friday"     >> return Friday
      , try $ string "saturday"   >> return Saturday
        
      , try $ string "firstweek"  >> return FirstWeek
      , try $ string "secondweek" >> return SecondWeek
      , try $ string "thirdweek"  >> return ThirdWeek
      , try $ string "fourthweek" >> return FourthWeek
        
      , try $ string "january"    >> return January
      , try $ string "february"   >> return February
      , try $ string "march"      >> return March
      , try $ string "april"      >> return April
      , try $ string "may"        >> return May
      , try $ string "june"       >> return June
      , try $ string "july"       >> return July
      , try $ string "august"     >> return August
      , try $ string "september"  >> return September
      , try $ string "october"    >> return October
      , try $ string "november"   >> return November
      , try $ string "december"   >> return December
        
      , try $ Interval <$> intervalInstantParser
        
      , try $ do spaces >> char '(' >> spaces
                 e <- exprParser
                 spaces >> char ')' >> spaces
                 return e
        
      , many (noneOf "") >>=
        (\a -> error $ "Error in function 'term': " ++ a)]

msgsParser :: Parser [String]
msgsParser =
  map (checkEmptyString "msgs" .
       substForNewLine .
       noSpaces) <$>
  sepBy
  (do txt <- many (noneOf ";.")
      spaces
      return txt)
  (char ';' >> spaces)

-------------------- Parser Helpers

instantParserP :: Parser Instant
instantParserP =
  choice [hour', hourMin', hourMin2', hourMin3', hourMin4',
          day', day2', dayMonth',
          dayMonthYear', dayHour', dayMonthHour',
          dayHourMin4', dayHourMin3', dayHourMin2', dayHourMin',
          dayMonthHourMin4', dayMonthHourMin3',
          dayMonthHourMin2', dayMonthHourMin',
          dayMonthYearHour',
          dayMonthYearHourMin4', dayMonthYearHourMin3',
          dayMonthYearHourMin2', dayMonthYearHourMin'] 
  where hour'                = try $ liftParsec hour
        hourMin'             = try $ liftParsec hourMin
        hourMin2'            = try $ liftParsec hourMin2
        hourMin3'            = try $ liftParsec hourMin3
        hourMin4'            = try $ liftParsec hourMin4
        day'                 = try $ liftParsec day
        day2'                = try $ liftParsec day2
        dayMonth'            = try $ liftParsec dayMonth
        dayMonthYear'        = try $ liftParsec dayMonthYear
        dayHour'             = try $ liftParsec dayHour
        dayMonthHour'        = try $ liftParsec dayMonthHour
        dayHourMin'          = try $ liftParsec dayHourMin
        dayHourMin2'         = try $ liftParsec dayHourMin2
        dayHourMin3'         = try $ liftParsec dayHourMin3
        dayHourMin4'         = try $ liftParsec dayHourMin4
        dayMonthHourMin'     = try $ liftParsec dayMonthHourMin
        dayMonthHourMin2'    = try $ liftParsec dayMonthHourMin2
        dayMonthHourMin3'    = try $ liftParsec dayMonthHourMin3
        dayMonthHourMin4'    = try $ liftParsec dayMonthHourMin4
        dayMonthYearHour'    = try $ liftParsec dayMonthYearHour
        dayMonthYearHourMin' = try $ liftParsec dayMonthYearHourMin
        dayMonthYearHourMin2'= try $ liftParsec dayMonthYearHourMin2
        dayMonthYearHourMin3'= try $ liftParsec dayMonthYearHourMin3
        dayMonthYearHourMin4'= try $ liftParsec dayMonthYearHourMin4

intervalDurParser :: Parser (Interval Dur)
intervalDurParser = choice [none, interval, orMore, exact]
  where none = try (string "none" >> return NoInterval)
        interval = try $ do
          from <- durParser
          spaces >> char '-' >> spaces
          to <- durParser
          return $ FromTo from to
        orMore = try $ do
          dur <- durParser
          spaces
          char '+'
          return $ OrMore dur
        exact = try (Exact <$> durParser)

intervalInstantParser :: Parser (Interval Instant)
intervalInstantParser = choice [none, interval, orMore, exact]
  where none = try (string "none" >> return NoInterval)
        interval = try $ do
          from <- instantParserP
          char '-' >> spaces
          to   <- instantParserP
          spaces
          return $ FromTo from to
        orMore = try $ do
          inst <- instantParserP
          char '+'
          spaces
          return $ OrMore inst
        exact = try (Exact <$> instantParserP)        

-------------------- Parsing Date Format Functions

hour :: String -> Maybe Instant
hour = propagMaybe PutToday (parseDate "%_Hh")

hourMin :: String -> Maybe Instant
hourMin = propagMaybe PutToday (parseDate "%_H:%_M")

hourMin2 :: String -> Maybe Instant
hourMin2 = propagMaybe PutToday (parseDate "%_Hh:%_M")

hourMin3 :: String -> Maybe Instant
hourMin3 = propagMaybe PutToday (parseDate "%_H:%_Mm")

hourMin4 :: String -> Maybe Instant
hourMin4 = propagMaybe PutToday (parseDate "%_Hh:%_Mm")

day :: String -> Maybe Instant
day = propagMaybe PutYearAndMonth (parseDate "%_d")

day2 :: String -> Maybe Instant
day2 = propagMaybe PutYearAndMonth (parseDate "%_dd")

dayMonth :: String -> Maybe Instant
dayMonth = propagMaybe PutYear (parseDate "%_d/%_m")

dayMonthYear :: String -> Maybe Instant
dayMonthYear = propagMaybe CompleteTime (parseDate "%_d/%_m/%Y")

dayHour :: String -> Maybe Instant
dayHour = propagMaybe PutYearAndMonth (parseDate "%_d %_Hh")

dayMonthHour :: String -> Maybe Instant
dayMonthHour = propagMaybe PutYear (parseDate "%_d/%_m %_Hh")

dayHourMin :: String -> Maybe Instant
dayHourMin = propagMaybe PutYearAndMonth (parseDate "%_d %_H:%_M")

dayHourMin2 :: String -> Maybe Instant
dayHourMin2 = propagMaybe PutYearAndMonth (parseDate "%_d %_Hh:%_M")

dayHourMin3 :: String -> Maybe Instant
dayHourMin3 = propagMaybe PutYearAndMonth (parseDate "%_d %_H:%_Mm")

dayHourMin4 :: String -> Maybe Instant
dayHourMin4 =propagMaybe PutYearAndMonth (parseDate "%_d %_Hh:%_Mm")

dayMonthHourMin :: String -> Maybe Instant
dayMonthHourMin = propagMaybe PutYear (parseDate "%_d/%_m %_H:%_M")

dayMonthHourMin2 :: String -> Maybe Instant
dayMonthHourMin2 =propagMaybe PutYear (parseDate "%_d/%_m %_Hh:%_M")

dayMonthHourMin3 :: String -> Maybe Instant
dayMonthHourMin3 =propagMaybe PutYear (parseDate "%_d/%_m %_H:%_Mm")

dayMonthHourMin4 :: String -> Maybe Instant
dayMonthHourMin4=propagMaybe PutYear (parseDate "%_d/%_m %_Hh:%_Mm")

dayMonthYearHour :: String -> Maybe Instant
dayMonthYearHour =
  propagMaybe CompleteTime (parseDate "%_d/%_m/%Y %_Hh")

dayMonthYearHourMin :: String -> Maybe Instant
dayMonthYearHourMin =
  propagMaybe CompleteTime (parseDate "%_d/%_m/%Y %_H:%_M")
  
dayMonthYearHourMin2 :: String -> Maybe Instant
dayMonthYearHourMin2 =
  propagMaybe CompleteTime (parseDate "%_d/%_m/%Y %_Hh:%_M")
  
dayMonthYearHourMin3 :: String -> Maybe Instant
dayMonthYearHourMin3 =
  propagMaybe CompleteTime (parseDate "%_d/%_m/%Y %_H:%_Mm")
  
dayMonthYearHourMin4 :: String -> Maybe Instant
dayMonthYearHourMin4 =
  propagMaybe CompleteTime (parseDate "%_d/%_m/%Y %_Hh:%_Mm")

-------------------- Helpers

liftParsec :: (String -> Maybe a) -> Parser a
liftParsec f = do
  txt <- many $ noneOf ".,+-()oa"
  case f (noSpaces txt) of
    Nothing -> fail $ "Fail with liftParsec. '" ++ txt ++ "'"
    Just rs -> return rs

checkEmptyString :: String -> String -> String
checkEmptyString cmd "" =
  error "Empty text in command " ++ cmd ++ "."
checkEmptyString _ s = s

ifNoNameError :: Show a => String -> (String,a) -> (String,a)
ifNoNameError cmd ("",a) =
  error $ "No name in command " ++ cmd ++ " (" ++ show a ++ ")."
ifNoNameError _ t = t

ifNoNameError2 :: (Show a, Show b) =>
                  String -> (String,a,b) -> (String,a,b)
ifNoNameError2 cmd ("",a,b) =
  error $ "No name in command " ++ cmd ++ show (a,b) ++ "."
ifNoNameError2 _ t = t

withArgsMeansError :: Command -> String -> Command
withArgsMeansError cmd "" = cmd
withArgsMeansError cmd _ =
  error $ "Argument in command " ++ show cmd ++ "."

parseDate :: String -> String -> Maybe ZonedTime
parseDate = parseTimeM True timeLocale
  where timeLocale = defaultTimeLocale {
          dateFmt = "%d/%m/%y",
          knownTimeZones = [TimeZone (-120) True "BRST"]
          }

propagMaybe :: (ZonedTime -> Instant)
            -> (String -> Maybe ZonedTime)
            ->  String -> Maybe Instant
propagMaybe p parse txt = case parse txt of
  Nothing -> Nothing
  Just ju -> Just $ p ju

substForNewLine :: String -> String
substForNewLine = map subst
  where subst '\\' = '\n'
        subst s = s
