{-# LANGUAGE ScopedTypeVariables #-}

module Constraint where

import Pitch
import Interval
import Music

data IntervalConstraint = IntervalConstraint Interval [Interval]
  deriving Show

data Constraint = IConstraint IntervalConstraint
  deriving Show

data Context = Context2 Location Location
  deriving Show

type Cont = (Context , Constraint)

secondSpeciesIntervals = [Min3, Maj3, Per5, Min6, Maj6, Per8]

checkIntervals2 :: Music (Location, PSym) -> [Cont]
checkIntervals2 (Music voices) =
  let vs = map flattenVoice voices
      xs = zip (vs !! 0) (vs !! 1)
  in map (\((l0,p0),(l1,p1)) ->
            (Context2 l0 l1, IConstraint (IntervalConstraint (opiSym (p0, p1))
                                         secondSpeciesIntervals)))
     xs
