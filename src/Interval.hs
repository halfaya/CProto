{-# LANGUAGE ScopedTypeVariables #-}

module Interval where

import Data.SBV

import Pitch
import Class

-- Unordered pitch interval
-- Absolute distance in semitones between two pitches.
type Upi  = Int8  -- should be non-negative
type SUpi = SInt8 -- should be non-negative

-- Ordered pitch interval
-- Signed distance in semitones between two pitches.
type Opi     = Int8
type SOpi    = SInt8
--type COpi    = IntC
type OpiPair = (Opi, Opi)

upi :: SPitchPair -> SUpi
upi (p1 , p2) = ite (p1 .< p2) (p2 - p1) (p1 - p2)

opi :: SPitchPair -> SOpi
opi (p1 , p2) = p2 - p1

data Interval =
  Per1  |
  Min2  |
  Maj2  |
  Min3  |
  Maj3  |
  Per4  |
  Aug4  |
  Per5  |
  Min6  |
  Maj6  |
  Min7  |
  Maj7  |
  Per8  |
  Min9  |
  Maj9  |
  Min10 |
  Maj10 |
  Per11 |
  Aug11 |
  Per12

iv :: Interval -> Opi
iv Per1  = 0
iv Min2  = 1
iv Maj2  = 2
iv Min3  = 3
iv Maj3  = 4
iv Per4  = 5
iv Aug4  = 6
iv Per5  = 7
iv Min6  = 8
iv Maj6  = 9
iv Min7  = 10
iv Maj7  = 11
iv Per8  = 12
iv Min9  = 13
iv Maj9  = 14
iv Min10 = 15
iv Maj10 = 16
iv Per11 = 17
iv Aug11 = 18
iv Per12 = 19

ivs :: Interval -> SInt8
ivs = literal . iv

intervalWithinOctave :: SUpi -> SUpi
intervalWithinOctave i = i `sMod` 12

isConsonant :: (IntC b a, CastC a) => a -> b
isConsonant i =
  (i ^== f Per1)  ^||
  (i ^== f Min3)  ^||
  (i ^== f Maj3)  ^||
  (i ^== f Per5)  ^||
  (i ^== f Min6)  ^||
  (i ^== f Maj6)  ^||
  (i ^== f Per8)  ^||
  (i ^== f Min10) ^||
  (i ^== f Maj10) ^||
  (i ^== f Per12)
  where f = cast . iv

isConsonant2 :: SOpi -> SBool
isConsonant2 i =
  (i .== ivs Per1)  .||
  (i .== ivs Min3)  .||
  (i .== ivs Maj3)  .||
  (i .== ivs Per5)  .||
  (i .== ivs Min6)  .||
  (i .== ivs Maj6)  .||
  (i .== ivs Per8)  .||
  (i .== ivs Min10) .||
  (i .== ivs Maj10) .||
  (i .== ivs Per12)
--  where i = intervalWithinOctave

isDissonant :: SOpi -> SBool
isDissonant = sNot . isConsonant

isPerfect :: SOpi -> SBool
isPerfect i =
  (i .== ivs Per1)  .||
  (i .== ivs Per4)  .||
  (i .== ivs Per5)  .||
  (i .== ivs Per8)  .||
  (i .== ivs Per12)
--  where i = intervalWithinOctave

isUnison :: SOpi -> SBool
isUnison i = i .== (ivs Per1)

-- Half or whole step.
isStep :: SOpi -> SBool
isStep i = (i .== ivs Min2) .|| (i .== ivs Maj2)

isThird :: SOpi -> SBool
isThird i = (i .== ivs Min3) .|| (i .== ivs Maj3)

isLeap :: SOpi -> SBool
isLeap i = (i .>= ivs Per4)

isPassing :: SPitch -> SPitch -> SPitch -> SBool
isPassing a b c =
  let i = opi (a , b)
      j = opi (b , c)
  in (isStep    i  .&& isStep    j) .||
     (isStep (- i) .&& isStep (- j))
