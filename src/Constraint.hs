{-# LANGUAGE ScopedTypeVariables #-}

module Constraint where

import Pitch
import Interval
import Music

data SetConstraint a = InSet a [a]
  deriving Show

data Constraint a = SConstraint (SetConstraint a)
  deriving Show

data Context = Context1 Location | Context2 Location Location
  deriving Show

type Cont a = (Context , Constraint a)

secondSpeciesIntervals = [Min3, Maj3, Per5, Min6, Maj6, Per8]

checkIntervals2 :: Music (Location, PSym) -> [Cont Interval]
checkIntervals2 (Music voices) =
  let vs = map flattenVoice voices
      xs = zip (vs !! 0) (vs !! 1)
  in map (\((l0,p0),(l1,p1)) ->
            (Context2 l0 l1,
             SConstraint (InSet (opiSym (p0, p1)) secondSpeciesIntervals)))
     xs
