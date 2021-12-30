{-# LANGUAGE ScopedTypeVariables #-}

module Pitch where

import Data.SBV

type Pitch      = Int8
type PitchClass = Int8 -- must be between 0 and 11 inclusive
type Octave     = Int8 -- C5 = middle C for Midi

type SPitch     = SInt8

type SPitchPair = (SPitch, SPitch)

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
