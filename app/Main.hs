module Main where

import Data
import Parser
import Command
import ProjectS
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
  let y = (map (\(_,x) -> projectType x))
          $ recognizeCommands projs
  print
    $ (map (\(n,a) -> (n,projectType a)))
    $ recognizeCommands projs
  ls <- mapM (\(n,projIO) -> do
                 proj <- projIO
                 return (n,proj)
             )
        $ map (\(n,cs) -> (n, makeProject cs))
        $ recognizeCommands projs
  newLine
  let ls' = read (show ls) :: [(Name, ProjectS)]
      equal = ls == ls'
  st <- getState
  let ls'' = mergeStates st ls'
  r<-infers ls'' y
  putStrLn "---------&&&&&---------"
  print y
  print equal
  putStrLn "---------&&&&&---------"
  print ls''
  putStrLn "---------&&&&&---------"
  print r
  putStrLn "---------&&&&&---------"
  return ()
  
