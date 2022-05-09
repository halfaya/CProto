{-# LANGUAGE ScopedTypeVariables #-}

module Pitch where

import Data.SBV

type Pitch      = Int8
type PitchClass = Int8         -- must be between 0 and 11 inclusive
type PitchPair  = (Pitch, Pitch)
type Octave     = Int8         -- C5 = middle C for Midi
type Scale      = [PitchClass]

type SPitch      = SInt8
type SPitchClass = SInt8    -- must be between 0 and 11 inclusive
type SPitchPair  = (SPitch, SPitch)
type SScale      = [SPitchClass]

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

inScale :: SScale -> SPitch -> SBool
inScale ps p =
  let p' = (p `sMod` 12)
  in foldr (\q b -> (p' .== q) .|| b) sFalse ps

-- C major scale, but can transpose to give other scales
majorScale :: SScale
majorScale = [0, 2, 4, 5, 7, 9, 11]

