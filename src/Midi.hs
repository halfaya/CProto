{-# LANGUAGE ScopedTypeVariables #-}

module Midi where

import System.Environment (getArgs)
import Codec.Midi 
import Data.List (sort, map)
import Text.Read (readMaybe)

import Pitch (Pitch)

type AbsTime      = Int
type TicksPerBeat = Int
type TrackName    = String

-- convert beats per minute to microseconds per beat
bpmToTempo :: Int -> Tempo
bpmToTempo bpm = round $ 1000000 * 60 / fromIntegral bpm

data MidiMessage = MNoteOn Velocity Ticks Key | MNoteOff Velocity Ticks Key
  deriving Eq

getTicks :: MidiMessage -> Ticks
getTicks (MNoteOn  _ t _) = t
getTicks (MNoteOff _ t _) = t

instance Ord MidiMessage where
  a <= b = getTicks a <= getTicks b

data MidiEvent = MidiEvent Pitch Ticks Ticks Velocity

eventToMessages :: MidiEvent -> [MidiMessage]
eventToMessages (MidiEvent p start stop v) =
  MNoteOn v start (fromIntegral p) : MNoteOff v stop (fromIntegral p) : [] 

-- Assumes all pitches have the same note duration and velocity
pitchesToEvents :: Ticks -> Velocity -> [Pitch] -> [MidiEvent]
pitchesToEvents d v ps = me 0 ps where
  me :: Ticks -> [Pitch] -> [MidiEvent]
  me t []       = []
  me t (p : ps) = MidiEvent p t (t + d) v : me (t + d) ps

pitchesToMessages :: Ticks -> Velocity -> [Pitch] -> [MidiMessage]
pitchesToMessages d v ps = concatMap eventToMessages (pitchesToEvents d v ps)

data MidiTrack = MidiTrack TrackName Preset Channel Tempo [MidiMessage]

makeTrack :: Channel -> AbsTime -> [MidiMessage] -> (Track Ticks , AbsTime)
makeTrack c t []                      = ([(0, TrackEnd)], t)
makeTrack c t (MNoteOn  v t' k : ms) = let (rest, t'') = makeTrack c t' ms
                                        in ((t' - t , NoteOn c k v) : rest, t'')
makeTrack c t (MNoteOff v t' k : ms) = let (rest, t'') = makeTrack c t' ms
                                        in ((t' - t, NoteOff c k v) : rest, t'')

toTrack :: MidiTrack -> Track Ticks
toTrack (MidiTrack name preset channel tempo messages) =
  (0, TrackName name) :
  (0, ProgramChange channel preset) :
  (0, TempoChange (bpmToTempo tempo)) :
  fst (makeTrack channel 0 (sort messages))

toMidi :: TicksPerBeat -> [MidiTrack] -> Midi
toMidi ticks tracks = let mtracks = map toTrack tracks
                      in Midi MultiTrack (TicksPerBeat ticks) mtracks

exportTracks :: String -> TicksPerBeat -> [MidiTrack] -> IO ()
exportTracks path ticksPerBeat tracks = do
  putStrLn $ "Writing file " ++ path
  --putStrLn $ show $ toMidi ticksPerBeat tracks
  exportFile path (toMidi ticksPerBeat tracks)

-- Instruments
piano :: Preset
piano = 0

marimba :: Preset
marimba = 12
