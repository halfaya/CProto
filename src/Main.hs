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

-- Expects variables to be of the form s0, s1, etc., so remove the "s" and sort via integer value.
getPitches :: SatResult -> [Pitch]
getPitches res = map fromIntegral (cvsToInt8s $ map snd (sortWith (r . tail . fst) (toList (getModelDictionary res))))
  where r :: String -> Int
        r = read

-- Expects variables to be of the form s0, s1, etc., so remove the "s" and sort via integer value.
-- Expects all strong beats first, followed by all weak beats. Last weak beat is superfluous so drop it.
getPitches2 :: SatResult -> [Pitch]
getPitches2 res = init1 $ interleave $ map fromIntegral (cvsToInt8s $ map snd (sortWith (r . tail . fst) (toList (getModelDictionary res))))
  where r :: String -> Int
        r = read
        interleave :: [Pitch] -> [Pitch] -- interleave first and second halves of a list
        interleave xs = foldr (\(a,b) cs -> a : b : cs) [] ((uncurry zip) (splitAt (length xs `div` 2) xs))
        init1 :: [a] -> [a]
        init1 [] = []
        init1 xs = init xs

tempo :: Int
tempo = 120

yVelocity = 60
cVelocity = 60

channel1 = 0
channel2 = 1

ticksPerBeat = 4 -- 4 means a tick is a 16th note
eighth       = 2 -- an 8th note is two ticks
sixteenth    = 1 -- a 16th note is one tick

yamanoteTrack :: MidiTrack
yamanoteTrack = MidiTrack "Cantus Firmus" piano channel1 tempo (pitchesToMessages eighth yVelocity yamanote)

midiFilename :: String
midiFilename = "/Users/leo/Downloads/test.mid"

main1 :: IO ()
main1 = do
  res <- makeCounterpoint cantusFirmus
  let cpTrack = MidiTrack "Counterpoint" marimba channel2 tempo (pitchesToMessages eighth cVelocity (getPitches res))
  let ycpTracks = cpTrack : yamanoteTrack : []
  exportTracks midiFilename ticksPerBeat ycpTracks
  putStrLn $ show $ getPitches res

main2 :: IO ()
main2 = do
  res <- makeCounterpoint2 cantusFirmus
  let cpTrack = MidiTrack "Counterpoint" marimba channel2 tempo (pitchesToMessages sixteenth cVelocity (getPitches2 res))
  let ycpTracks = cpTrack : yamanoteTrack : []
  exportTracks midiFilename ticksPerBeat ycpTracks
  putStrLn $ show $ getPitches2 res

main :: IO ()
main = main2
