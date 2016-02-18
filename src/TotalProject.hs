module TotalProject where

import Data
{-
infer :: Name -> Project -> TotalProject'
infer name proj = case proj of
  CD c d as ->
    TotalProject'
    (makeCycle c as)
    (makeDur d as)
    (makeEndpoint (inferEndpointCD c d))
    (makeTotaltime (inferTotaltimeD d))
    (addOns' as)
  CD' c d' as ->
    TotalProject'
    (makeCycle c as)
    (makeDur d' as)
    Maximum
    Infinite
    (addOns' as)
  DE d e as ->
    TotalProject'
    (makeCycle (inferCycleDE d e) as)
    (makeDur d as)
    (makeEndpoint e)
    (makeTotaltime (inferTotaltimeD d))
    (addOns' as)
  CE c e as ->
    let d = estimateDurCE c e
    in TotalProject'
       (makeCycle c as)
       (makeDur d as)
       (makeEndpoint e)
       (makeTotaltime (inferTotaltimeD d))
       (addOns' as)
  ET e t as ->
    TotalProject'
    (makeCycle (inferCycleET e t) as)
    (makeDur (estimateDurET e t) as)
    (makeEndpoint e)
    (makeTotaltime t)
    (addOns' as)
  CT c t as ->
    TotalProject'
    (makeCycle c as)
    (makeDur (inferDurCT c t) as)
    (makeEndpoint (inferEndpointCT c t))
    (makeTotaltime t)
    (addOns' as)
  CDE c d e as ->
    if redundancyCDE c d e
    then error "Redundancy."
    else TotalProject'
         (makeCycle c as)
         (makeDur d as)
         (makeEndpoint e)
         (makeTotaltime (inferTotaltimeD d))
         (addOns' as)
  CD'E c d' e as ->
    let t = inferTotaltimeCE c e
    in TotalProject'
       (makeCycle c as)
       (truncateDurT (makeDur d' as) t)
       (makeEndpoint e)
       (makeTotaltime t)
       (addOns' as)
  CDT c d t as ->
    if redundancyDT d t
    then error "Redundancy."
    else TotalProject'
         (makeCycle c as)
         (makeDur d as)
         (makeEndpoint (inferEndpointCT c t))
         (makeTotaltime t)
         (addOns' as)
  CD'T c d' t as ->
    TotalProject'
    (makeCycle c as)
    (truncateDurT (makeDur d' as) t)
    (makeEndpoint (inferEndpointCT c t))
    (makeTotaltime t)
    (addOns' as)
  CET c e t as ->
    TotalProject'
    (makeCycle c as)
    (makeDur (inferDurCET c e t) as)
    (makeEndpoint e)
    (makeTotaltime t)
    (addOns' as)
  DET d e t as ->
    if redundancyDT d t
    then error "Redundancy."
    else TotalProject'
         (makeCycle (inferCycleDE d e) as)
         (makeDur d as)
         (makeEndpoint e)
         (makeTotaltime t)
         (addOns' as)
  D'ET d' e t as ->
    TotalProject'
    (makeCycle (inferCycleD'E d' e) as)
    (truncateDurT (makeDur d' as) t)
    (makeEndpoint e)
    (makeTotaltime t)
    (addOns' as)
  CDET c d e t as ->
    case (redundancyDT d t, redundancyCDE c d e) of
      (True,   True) -> error "Redundancies."
      (True,  False) -> error "Redundancy: DT"
      (False,  True) -> error "Redundancy: CDE"
      (False, False) -> TotalProject'
                        (makeCycle c as)
                        (makeDur d as)
                        (makeEndpoint e)
                        (makeTotaltime t)
                        (addOns' as)
  CD'ET c d' e t as ->
    TotalProject'
    (makeCycle c as)
    (truncateDurT (makeDur d' as) t)
    (makeEndpoint e)
    (makeTotaltime t)
    (addOns' as)

---------------------
  
makeDur :: Duration -> AddOns -> Duration'
makeDur = undefined

makeCycle :: Cycle -> AddOns -> Cycle'
makeCycle = undefined

makeEndpoint :: Endpoint -> Endpoint'
makeEndpoint = Endpoint' . fromEndpoint

makeTotaltime :: Totaltime -> Totaltime'
makeTotaltime = Finite

truncateDurT = undefined
inferCycleCD'E = undefined
redundancyCDE = undefined
redundancyDT = undefined
inferCycleD'E = undefined
inferCycleDE = undefined
inferDurCET = undefined
truncateDE = undefined
maxDuration = undefined
estimateDurCE = undefined
inferCycleET = undefined
estimateDurET = undefined
inferDurCT = undefined
addOns' = undefined
inferEndpointCD = undefined
inferEndpointCT = undefined
inferTotaltimeD = undefined
inferTotaltimeCE = undefined

---------- Helpers

fromEndpoint (Endpoint e) = e

conversion :: TotalProject' -> TotalProject
conversion (TotalProject' c d _e _tt as) = TotalProject c d as
-}
