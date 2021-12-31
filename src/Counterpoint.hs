{-# LANGUAGE ScopedTypeVariables #-}

module Counterpoint where

import Data.SBV

import Pitch
import Interval

makeCounterpoint :: [SPitch] -> IO SatResult
makeCounterpoint cantusFirmus = sat $ do
  ps <- mkExistVars (length cantusFirmus) :: Symbolic [SPitch]
  let pairs = zip cantusFirmus ps :: [SPitchPair]
  constrain $ numContrary pairs .>= 24
  constrain $ sNot (repeatedNote ps)
  solve $ firstSpecies pairs

-- Assumes input length is at least 3
-- Cantus firmus is first pair; counterpoint is always higher.
firstSpecies :: [SPitchPair] -> [SBool]
firstSpecies pps =
  let start  = head pps
      middle = tail (init pps)
      end    = (last middle , last pps)

      scaleOk = map (inScale majorScale . snd) pps 
  
      startOk = checkStart start
      intervalsOk = map checkInterval middle
      motionOk = checkMotion pps
      endOk = checkEnd end
  in startOk ++ intervalsOk ++ motionOk ++ endOk ++ scaleOk

-- Counterpoint must be a unison, perfect 5th or perfect octave above cantus firmus.
checkStart :: SPitchPair -> [SBool]
checkStart pp =
  let o = opi pp
  in (o .== per1 .|| o .== per5 .|| o .== per8) : []

-- Note that the cantus firmus must end by a half step up or full step down.
-- If this is not satisfied the generation of counterpoint will fail.
checkEnd :: (SPitchPair , SPitchPair) -> [SBool]
checkEnd ((p1, q1), (p2, q2)) =
  let c1 = opi (p2, q2) .== per8 -- counterpoint ends a perfect octave above the cantus firmus
      c2 = opi (p1, p2) .== min2 .&& opi (q2, q1) .== maj2 -- cf half step up and cp full step down
      c3 = opi (p2, p1) .== maj2 .&& opi (q1, q2) .== min2 -- cf full step up and cp half step down
  in c1 : (c2 .|| c3): []

checkInterval :: SPitchPair -> SBool
checkInterval pp = let o = opi pp in isConsonant o .&& sNot (isUnison o)

parallel :: (SPitchPair , SPitchPair) -> SBool
parallel ((p1, q1), (p2, q2)) = opi (p1, p2) .== opi (q1, q2) 

similar :: (SPitchPair , SPitchPair) -> SBool
similar pps@((p1, q1), (p2, q2)) =
  ((p1 .< p2 .&& q1 .< q2) .|| (p1 .> p2 .&& q1 .> q2))
  .&& sNot (parallel pps)

similarOrParallel :: (SPitchPair , SPitchPair) -> SBool
similarOrParallel ((p1, q1), (p2, q2)) =
  (p1 .< p2 .&& q1 .< q2) .||
  (p1 .> p2 .&& q1 .> q2) .||
  (p1 .== p2 .&& q1 .== q2)

contrary :: (SPitchPair , SPitchPair) -> SBool
contrary ((p1, q1), (p2, q2)) =
  (p1 .< p2 .&& q1 .> q2) .||
  (p1 .> p2 .&& q1 .< q2)

oblique :: (SPitchPair , SPitchPair) -> SBool
oblique ((p1, q1), (p2, q2)) =
  (p1 .== p2 .&& q1 ./= q2) .||
  (p1 ./= p2 .&& q1 .== q2)

checkMotionPair :: (SPitchPair , SPitchPair) -> SBool
checkMotionPair pps =
  sNot (isPerfect (opi (snd pps)) .&& similarOrParallel pps)

checkMotion :: [SPitchPair] -> [SBool]
checkMotion []                = []
checkMotion (_ : [])          = []
checkMotion (pp1 : pp2 : pps) = checkMotionPair (pp1, pp2) : checkMotion (pp2 : pps)

numContrary :: [SPitchPair] -> SInteger
numContrary []                = 0
numContrary (_ : [])          = 0
numContrary (pp1 : pp2 : pps) = (ite (contrary (pp1, pp2)) 1 0) + numContrary (pp2 : pps)

repeatedNote :: [SPitch] -> SBool
repeatedNote []             = sFalse
repeatedNote (_ : [])       = sFalse
repeatedNote (p1 : p2 : ps) = ite (p1 .== p2) sTrue (repeatedNote (p2 :ps))
