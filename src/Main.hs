{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.SBV
import System.Directory (getHomeDirectory)

import Counterpoint
import Midi
import Pitch
import Interval
import Yamanote
import Frog

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

generateCounterpoint :: Species -> [Pitch] -> IO ()
generateCounterpoint species cantusFirmus = do
  let cfTrack = cantusFirmusTrack cantusFirmus
  res <- makeCounterpoint species (map literal cantusFirmus)
  let cpPitches = getPitches species res
  let cpTrack = MidiTrack "Counterpoint" marimba channel2 tempo (pitchesToMessages (noteLength species) cpVelocity cpPitches)
  let fcpTracks = cpTrack : cfTrack : []
  midiFilename <- getMidiFilename
  exportTracks midiFilename ticksPerBeat fcpTracks
  putStrLn $ show $ cpPitches

main :: IO ()
main = generateCounterpoint Second frog
