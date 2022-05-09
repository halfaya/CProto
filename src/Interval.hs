{-# LANGUAGE ScopedTypeVariables #-}

module Interval where

import Data.SBV

import Pitch

-- Unordered pitch interval
-- Absolute distance in semitones between two pitches.
type Upi  = Int8  -- should be non-negative
type SUpi = SInt8 -- should be non-negative

-- Ordered pitch interval
-- Signed distance in semitones between two pitches.
type Opi     = Int8
type SOpi    = SInt8
type OpiPair = (Opi, Opi)

upi :: SPitchPair -> SUpi
upi (p1 , p2) = ite (p1 .< p2) (p2 - p1) (p1 - p2)

opi :: SPitchPair -> SOpi
opi (p1 , p2) = p2 - p1

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

intervalWithinOctave :: SUpi -> SUpi
intervalWithinOctave i = i `sMod` 12

isConsonant :: SOpi -> SBool
isConsonant i =
  (i .== per1)  .||
  (i .== min3)  .||
  (i .== maj3)  .||
  (i .== per5)  .||
  (i .== min6)  .||
  (i .== maj6)  .||
  (i .== per8)  .||
  (i .== min10) .||
  (i .== maj10) .||
  (i .== per12)
--  where i = intervalWithinOctave iv

isDissonant :: SOpi -> SBool
isDissonant = sNot . isConsonant

isPerfect :: SOpi -> SBool
isPerfect i =
  (i .== per1)  .||
  (i .== per4)  .||
  (i .== per5)  .||
  (i .== per8)  .||
  (i .== per12)
--  where i = intervalWithinOctave iv

isUnison :: SOpi -> SBool
isUnison i = i .== per1

-- Half or whole step.
isStep :: SOpi -> SBool
isStep i = (i .== min2) .|| (i .== maj2)

isThird :: SOpi -> SBool
isThird i = (i .== min3) .|| (i .== maj3)

isLeap :: SOpi -> SBool
isLeap i = (i .>= per4)

isPassing :: SPitch -> SPitch -> SPitch -> SBool
isPassing a b c =
  let i = opi (a , b)
      j = opi (b , c)
  in (isStep    i  .&& isStep    j) .||
     (isStep (- i) .&& isStep (- j))
  
  
