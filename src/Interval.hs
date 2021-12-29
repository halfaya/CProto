{-# LANGUAGE ScopedTypeVariables #-}

module Interval where

import Data.SBV

import Pitch

-- Unordered pitch interval
-- Absolute distance in semitones between two pitches.
type Upi = SWord8

upi :: (SPitch , SPitch) -> Upi
upi (p1 , p2) = ite (p1 .< p2) (p2 - p1) (p1 - p2)

-- Names for intervals
per1  = 0
min2  = 1
maj2  = 2
min3  = 3
maj3  = 4
per4  = 5
aug4  = 6
per5  = 7
min6  = 8
maj6  = 9
min7  = 10
maj7  = 11
per8  = 12
min9  = 13
maj9  = 14
min10 = 15
maj10 = 16
per11 = 17
aug11 = 18
per12 = 19

intervalWithinOctave :: Upi -> Upi
intervalWithinOctave i = i `sMod` 12

isConsonant :: Upi -> SBool
isConsonant iv =
  (i .== per1)  .||
  (i .== min3)  .||
  (i .== maj3)  .||
  (i .== per5)  .||
  (i .== min6)  .||
  (i .== maj6)  .||
  (i .== per8)
  where i = intervalWithinOctave iv

isDissonant :: Upi -> SBool
isDissonant = sNot . isConsonant

isPerfect :: Upi -> SBool
isPerfect iv =
  (i .== per1)  .||
  (i .== per4)  .||
  (i .== per5)  .||
  (i .== per8)
  where i = intervalWithinOctave iv

isUnison :: Upi -> SBool
isUnison i = i .== per1

isThird :: Upi -> SBool
isThird i = (i .== min3) .|| (i .== maj3)

-- Half or whole step.
isStep :: Upi -> SBool
isStep i =
  (i .== min2)  .||
  (i .== maj2)
