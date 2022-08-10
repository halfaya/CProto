{-# LANGUAGE ScopedTypeVariables #-}

module Interval where

import Prelude hiding ((==), (/=), (<=), (<), (>=), (>), not, (||), (&&))
import Data.SBV

import Pitch
import Class

-- Ordered pitch interval
-- Signed distance in semitones between two pitches.
type Opi     = Int8

opi :: (Num a) => (a , a) -> a
opi (p1 , p2) = p2 - p1

opiSym :: (PSym, PSym) -> Interval
opiSym (p , q) = vi (opi (psymToPitch p, psymToPitch q))

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
  deriving (Eq, Show)

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

vi :: Opi -> Interval
vi 0  = Per1
vi 1  = Min2
vi 2  = Maj2
vi 3  = Min3
vi 4  = Maj3
vi 5  = Per4
vi 6  = Aug4
vi 7  = Per5
vi 8  = Min6
vi 9  = Maj6
vi 10 = Min7
vi 11 = Maj7
vi 12 = Per8
vi 13 = Min9
vi 14 = Maj9
vi 15 = Min10
vi 16 = Maj10
vi 17 = Per11
vi 18 = Aug11
vi 19 = Per12

--intervalWithinOctave :: SUpi -> SUpi
--intervalWithinOctave i = i `sMod` 12

isConsonant :: (IntC bool interval, FromInt8 interval) => interval -> bool
isConsonant i =
  (i == f Per1)  ||
  (i == f Min3)  ||
  (i == f Maj3)  ||
  (i == f Per5)  ||
  (i == f Min6)  ||
  (i == f Maj6)  ||
  (i == f Per8)--  ||
--  (i == f Min10) ||
--  (i == f Maj10) ||
--  (i == f Per12)
  where f = fromInt8 . iv

isDissonant :: (IntC bool interval, FromInt8 interval) => interval -> bool
isDissonant = not . isConsonant

isPerfect :: (IntC bool interval, FromInt8 interval) => interval -> bool
isPerfect i =
  (i == f Per1)  ||
  (i == f Per4)  ||
  (i == f Per5)  ||
  (i == f Per8)  ||
  (i == f Per11)  ||
  (i == f Per12)
  where f = fromInt8 . iv

isUnison :: (IntC bool interval, FromInt8 interval) => interval -> bool
isUnison i = i == (f Per1)
  where f = fromInt8 . iv

is158 :: (IntC bool interval, FromInt8 interval) => interval -> bool
is158 i =
  (i == f Per1)  ||
  (i == f Per5)  ||
  (i == f Per8)
  where f = fromInt8 . iv

-- Half or whole step.
isStep :: (IntC bool interval, FromInt8 interval) => interval -> bool
isStep i = (i == f Min2) || (i == f Maj2)
  where f = fromInt8 . iv

isThird :: (IntC bool interval, FromInt8 interval) => interval -> bool
isThird i = (i == f Min3) || (i == f Maj3)
  where f = fromInt8 . iv

isLeap :: (IntC bool interval, FromInt8 interval) => interval -> bool
isLeap i = (i >= p4) || (i <= -p4)
  where p4 = (fromInt8 . iv) Per4

checkInterval :: (IntC b a, FromInt8 a) => (a, a) -> b
checkInterval pp = let i = opi pp in isConsonant i && not (isUnison i)

-- Perfect 4 is okay
checkInterval4 :: (IntC b a, FromInt8 a) => (a, a) -> b
checkInterval4 pp = let i = opi pp in (isConsonant i || i == (fromInt8 . iv) Per4) && not (isUnison i)

{-
isPassing :: SPitch -> SPitch -> SPitch -> SBool
isPassing a b c =
  let i = opi (a , b)
      j = opi (b , c)
  in (isStep    i  && isStep    j) ||
     (isStep (- i) && isStep (- j))
-}
