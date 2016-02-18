module Main where

import Data
import Parser
import Command
import Project

newLine = putStrLn ""

main :: IO ()
main = do
  putStr "File: "
  file <- getLine
  projs <- parseFile projects file
  newLine
  print projs
  newLine
  print $ recognizeCommands projs
  newLine
  print
    $ (map (\(n,a) -> (n,projectType a)))
    $ recognizeCommands  projs
  ls <- mapM (\(n,projIO) -> do
                 proj <- projIO
                 return (n,proj)
             )
        $ map (\(n,cs) -> (n, makeProject cs))
        $ recognizeCommands  projs
  newLine
  let ls' = read (show ls) :: [(Name, Project)]
      equal = ls == ls'
  print equal
  newLine
  print ls'
  return ()
  
