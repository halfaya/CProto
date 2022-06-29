{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.SBV
import System.Directory (getHomeDirectory)

import Beethoven
import Constraint
import Counterpoint
import Frog
import Interval
import Midi
import Music
import Pitch
import Yamanote

tempo :: Int
tempo = 120

cfVelocity = 60 -- velocity of cantus firmus
cpVelocity = 60 -- velocity of counterpoint

channel1 = 0
channel2 = 1

ticksPerBeat = 4 -- 4 means a tick is a 16th note
eighth       = 2 -- an 8th note is two ticks
sixteenth    = 1 -- a 16th note is one tick

noteLength :: Species -> Int
noteLength First  = eighth
noteLength Second = sixteenth

cantusFirmusTrack :: [Pitch] -> MidiTrack
cantusFirmusTrack cantusFirmus = MidiTrack "Cantus Firmus" piano channel1 tempo (pitchesToMessages eighth cfVelocity cantusFirmus)

midiFilenameRelativePath :: String
midiFilenameRelativePath = "/Music/MusicTools/test.mid"

getMidiFilename :: IO String
getMidiFilename = fmap (++ midiFilenameRelativePath) getHomeDirectory

generateCounterpoint :: Species -> Music MPitch -> IO ()
generateCounterpoint species music = do
  res <- makeCounterpoint1 music
  let m = flattenMusic (getPitches res music) :: [[Pitch]]
  let (v0, v1) = (m !! 0, m !! 1) :: ([Pitch], [Pitch])
  let cfTrack = cantusFirmusTrack v0 -- Assume v0 is cantus firmus for now
  let cpTrack = MidiTrack "Counterpoint" marimba channel2 tempo (pitchesToMessages (noteLength species) cpVelocity v1)
  let fcpTracks = cpTrack : cfTrack : []
  midiFilename <- getMidiFilename
  exportTracks midiFilename ticksPerBeat fcpTracks
  putStrLn $ show $ v0
  putStrLn $ show $ v1

toFirstSpeciesInput :: FirstSpecies2 (Maybe Pitch) -> Music MPitch
toFirstSpeciesInput = mapMusic toMPitch . indexMusic . firstSpecies2toMusic 

main :: IO ()
main = generateCounterpoint First (toFirstSpeciesInput beethoven146cf)

-- temporary tests
test1 =
  (checkIntervals2 . indexMusic . unmaybeMusic . firstSpecies2toMusic) beethoven146a2

