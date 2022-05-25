{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.SBV
import System.Directory (getHomeDirectory)

import Beethoven
import Counterpoint
import Frog
import Interval
import Midi
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

generateCounterpoint :: Species -> [[MPitch]] -> IO ()
generateCounterpoint species music = do
  res <- makeCounterpoint1 music
  let ps = map toPair (getPitches res music) :: [PitchPair]
  let cantusFirmus = map fst ps :: [Pitch]
  let cpPitches = map snd ps
  let cfTrack = cantusFirmusTrack cantusFirmus
  let cpTrack = MidiTrack "Counterpoint" marimba channel2 tempo (pitchesToMessages (noteLength species) cpVelocity cpPitches)
  let fcpTracks = cpTrack : cfTrack : []
  midiFilename <- getMidiFilename
  exportTracks midiFilename ticksPerBeat fcpTracks
  putStrLn $ show $ cantusFirmus
  putStrLn $ show $ cpPitches

toFirstSpeciesInput :: [Pitch] -> [[MPitch]]
toFirstSpeciesInput ps = toMPitches (map (\p -> [Just p, Nothing]) ps)

main :: IO ()
main = generateCounterpoint First (toMPitches beethoven146cf) --(toFirstSpeciesInput yamanote)
