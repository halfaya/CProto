{-# LANGUAGE ScopedTypeVariables #-}

module Pitch where

import Data.SBV hiding (fromBool)
import Prelude  hiding ((==), (||))

import Class

type Pitch      = Int8
type PitchClass = Int8            -- must be between 0 and 11 inclusive
type PitchPair  = (Pitch, Pitch)
type Octave     = Int8            -- C5 = middle C for Midi
type Scale      = [PitchClass]

type SPitch      = SInt8
type SPitchPair  = (SPitch, SPitch)

standardMidiPitch :: PitchClass -> Octave -> Pitch
standardMidiPitch p o = o * 12 + p

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

inScale :: (IntC bool pitch, FromInt8 pitch, SDivisible pitch) => Scale -> pitch -> bool
inScale scale p =
  let p' = (p `sMod` 12)
  in foldr (\q b -> (p' == q) || b) false (map fromInt8 scale)

-- C major scale, but can transpose to give other scales
majorScale :: Scale
majorScale = [0, 2, 4, 5, 7, 9, 11]

-- Accidental
data PAcc = Fl | Na | Sh

instance Show PAcc where
  show Fl  = "♭"
  show Na = "" --"♮"
  show Sh  = "♯"

-- Symbolic pitch class
data PC = C PAcc | D PAcc | E PAcc | F PAcc | G PAcc | A PAcc | B PAcc

instance Show PC where
  show (C a) = "C" ++ show a
  show (D a) = "D" ++ show a
  show (E a) = "E" ++ show a
  show (F a) = "F" ++ show a
  show (G a) = "A" ++ show a
  show (A a) = "B" ++ show a
  show (B a) = "C" ++ show a

-- Symbolic pitch
type PSym = (PC , Octave)

paccToMod :: PAcc -> Int8
paccToMod Fl = -1
paccToMod Na = 0
paccToMod Sh = 1

pcToPitch :: PC -> Pitch
pcToPitch (C a) = 0  + paccToMod a
pcToPitch (D a) = 2  + paccToMod a
pcToPitch (E a) = 4  + paccToMod a
pcToPitch (F a) = 5  + paccToMod a
pcToPitch (G a) = 7  + paccToMod a
pcToPitch (A a) = 9  + paccToMod a
pcToPitch (B a) = 11 + paccToMod a

psymToPitch :: PSym -> Pitch
psymToPitch (pc, o) = pcToPitch pc + o * 12
