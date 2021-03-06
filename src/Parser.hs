module Parser where

------------------- Imports

import Data
import Text.Parsec
import Data.Char (toLower)

------------------- Parse Text

parseTextEither :: Parser a -> String -> Either ParseError a
parseTextEither parser text = parse parser "Error" text

parseTextMaybe :: Parser a -> String -> Maybe a
parseTextMaybe par = eitherToMaybe . parseTextEither par
  where
    eitherToMaybe (Left    _) = Nothing
    eitherToMaybe (Right res) = Just res

parseText :: Parser a -> String -> a
parseText par = eitherOut . parseTextEither par
  where
    eitherOut :: (Show err) => Either err a -> a
    eitherOut (Left  err) = error (show err)
    eitherOut (Right res) = res

------------------- Parse File
    
parseFileEither :: Parser a -> File -> IO (Either ParseError a)
parseFileEither parser file = do
  contents <- readFile file
  return $ parseTextEither parser contents

parseFileMaybe :: Parser a -> File -> IO (Maybe a)
parseFileMaybe parser file = do
  contents <- readFile file
  return $ parseTextMaybe parser contents

parseFile :: Parser a -> File -> IO a
parseFile parser file = do
  contents <- readFile file
  return $ parseText parser contents

------------------- Parser

command :: Parser (Keyword,Arguments)
command = both (toLowerCase . noSpaces) <$> do
  instr <- many (noneOf " ")
  spaces
  args  <- many (noneOf "&.")
  return (instr,args)
  where
    both :: (a -> b) -> (a,a) -> (b,b)
    both f (a,b) = (f a, f b)

project :: Parser Parsed
project = check <$> projectName (toLowerCase . noSpaces) <$> do
  name <- many (noneOf "=")
  spaces
  string "=>"
  spaces
  comms <- command `sepBy` (char '&' >> spaces)
  spaces
  char '.'
  return (Parsed name comms)
  where
    projectName :: (Name -> Name) -> Parsed -> Parsed
    projectName f (Parsed name a) = Parsed (f name) a

    check :: Parsed -> Parsed
    check (Parsed n []) =
      error $ "Project '" ++ n ++ "' without definitions."
    check p = p

projects :: Parser [Parsed]
projects = do
  spaces
  many $ do
    p <- project
    spaces
    return p

------------------- Helpers
    
toLowerCase :: String -> String
toLowerCase = map toLower

noSpaces :: String -> String
noSpaces =
  reverse
  . noSpacesAtBeggining
  . reverse
  . noSpacesAtBeggining
  where
    noSpacesAtBeggining (' ':xs) = noSpacesAtBeggining xs
    noSpacesAtBeggining s        = s
