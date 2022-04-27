{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module Types where

import Data.Maybe (catMaybes)

data Pitch where
  Pitch :: Int -> Pitch
  deriving Show

type PitchPair = (Pitch, Pitch)

-- OPI
data Interval where
  Interval :: Int -> Interval
  deriving (Show, Eq)

pitchPairInterval :: PitchPair -> Interval
pitchPairInterval (Pitch a , Pitch b) =
  let n = b - a
  in Interval (if n > 12 then n `mod` 12 else n)

data IntervalSet where
  ConsonantInterval :: IntervalSet
  deriving Show

intervalSet :: IntervalSet -> [Interval]
intervalSet ConsonantInterval =
  [Interval 0,  -- perfect unison
   Interval 3,  -- minor third
   Interval 4,  -- major third
   Interval 7,  -- perfect fifth
   Interval 8,  -- minor sixth
   Interval 9,  -- major sixth
   Interval 12] -- perfect octave

type IntervalPair = (Interval, Interval)

data Music where
  MPitch :: Pitch -> Music
  MInterval :: Pitch -> Music
  Pair :: Music -> Music -> Music

data Constraint where
  IntervalInSet :: IntervalSet -> Interval -> Constraint
  deriving Show

data Error where
  ConstraintFail :: Constraint -> Error
  deriving Show

firstSpecies :: [PitchPair] -> [Constraint]
firstSpecies = map (IntervalInSet ConsonantInterval . pitchPairInterval)

check :: Constraint -> Maybe Error
check c@(IntervalInSet s i) = let x = intervalSet s in
  if i `elem` x then Nothing else Just (ConstraintFail c)

type PitchClass = Int
type Octave     = Int

standardMidiPitch :: PitchClass -> Octave -> Pitch
standardMidiPitch p o = Pitch (o * 12 + p)

c :: Octave -> Pitch -- etc
c  = standardMidiPitch 0
db = standardMidiPitch 1
d  = standardMidiPitch 2
eb = standardMidiPitch 3
e  = standardMidiPitch 4
f  = standardMidiPitch 5
gb = standardMidiPitch 6
g  = standardMidiPitch 7
ab = standardMidiPitch 8
a  = standardMidiPitch 9
bb = standardMidiPitch 10
b  = standardMidiPitch 11

scarlatti :: [PitchPair]
scarlatti =
  [(d 5 , ab 5) ,
   (c 5 , a 5) ,
   (b 4 , b 5) ,
   (a 4 , c 6) ,
   (b 4 , d 6) ,
   (c 5 , e 6) ,
   (d 5 , f 6) ,
   (e 4 , ab 5) ,
   (d 5 , a 5) ,
   (c 5 , e 5) ,
   (d 5 , d 6) ,
   (e 5 , c 6) ,
   (d 5 , d 6) ,
   (e 5 , ab 5) ,
   (e 4 , b 5) ,
   (a 4 , a 5) ]

test = catMaybes (map check (firstSpecies scarlatti))
