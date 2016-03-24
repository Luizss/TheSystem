module Main where

import Prelude hiding (truncate)

import Data
import Parser
import Command
import ProjectS
import StateAndInfer
import Project
import Activities
import Scheduler

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
--  print y
  print equal
  putStrLn "---------&&&&&---------"
--  print ls''
  putStrLn "---------&&&&&---------"
  print r
  putStrLn "---------&&&&&---------"
  ts <- transforms r
  print ts
  putStrLn "---------&&&&&---------"
  let g = projectsInOrder ts
  print g
  putStrLn "---------&&&&&---------"
  print $ help g == help ts
  print $ {-map (\x -> (getName x, head (getOkInterval x))) $ map (\x -> (getName x, getId x, getConstraints x)) $-} activitiesFromProjects [g !! 4] --ts
  (ts',ini) <- stripPasts $ activitiesFromProjects [g !! 4]
  putStrLn "---------&&&&&---------"
  print $ {-map (\x -> (getName x, head (getOkInterval x))) $ map (\x -> (getName x, getId x, getConstraints x)) $-} ts' --ts
  let yt = limitSchedule $ scheduler ini ts'
  putStrLn "---------&&&&&---------"
  print (filter (\(b,t) -> b) (leaves yt))
{-  asd <- mapM
         (\(n,l) -> do l' <- truncate l
                       return (n,stdDur (Just l')))
         r
  print asd-}
  return ()

{-stdDur :: DurationS -> DurStd --[[Minutes]]
stdDur d = case d of
  DurationS    lst -> toStd lst
  DurationInfS lst -> toStd lst
  where
    toStd = map (fromIntegral
                 . sum
                 . map durToMinutes
                 . takeDurations)
    takeDurations (_,l) = map (\(_,d,_) -> d) l
-}

help = map (\(n,x) -> n)
