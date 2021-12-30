{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Map (toList)
import Data.SBV
import Data.SBV.Internals (CV)
import GHC.Exts (sortWith)

import Counterpoint
import Midi
import Pitch
import Interval
import Yamanote

cvsToInt8s :: [CV] -> [Int8]
cvsToInt8s [] = []
cvsToInt8s cvs = case parseCVs cvs of
  Nothing         -> []
  Just (w , cvs') -> w : cvsToInt8s cvs'

-- Expects variables to be of the form s0, s1, etc., so remove the "s" and sort via integer value
getPitches :: SatResult -> [Pitch]
getPitches res = map fromIntegral (cvsToInt8s $ map snd (sortWith (r . tail . fst) (toList (getModelDictionary res))))
  where r :: String -> Int
        r = read

tempo :: Int
tempo = 120

yVelocity = 60
cVelocity = 60

channel1 = 0
channel2 = 1

ticksPerBeat = 4 -- 4 means a tick is a 16th note
eighth       = 2 -- an 8th note is two ticks

yamanoteTrack :: MidiTrack
yamanoteTrack = MidiTrack "Cantus Firmus" piano channel1 tempo (pitchesToMessages eighth yVelocity yamanote)

midiFilename :: String
midiFilename = "/Users/leo/Downloads/test.mid"

main :: IO ()
main = do
  res <- makeCounterpoint cantusFirmus
  let cpTrack = MidiTrack "Counterpoint" marimba channel2 tempo (pitchesToMessages eighth cVelocity (getPitches res))
  let ycpTracks = cpTrack : yamanoteTrack : []
  exportTracks midiFilename ticksPerBeat ycpTracks
  putStrLn $ show $ getPitches res
