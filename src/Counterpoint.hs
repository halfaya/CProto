{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Counterpoint where

import Data.List.Split (splitOn)
import Data.Map (toList)
import Data.SBV hiding (ite)
import GHC.Exts (sortWith)
import Prelude hiding ((==), (/=), (<=), (<), (>=), (>), not, (||), (&&))

import Boundary
import Class
import Interval
import Pitch
import Interval
import Motion
import Music

toVarName :: (Int, Int, Int) -> String
toVarName (voice, bar, beat) = show voice ++ "_" ++ show bar ++ "_" ++ show beat

fromVarName :: String -> (Int, Int, Int)
fromVarName s = case splitOn "_" s of
  [voice , bar, beat] -> (read voice , read bar, read beat)
  _       -> (-1, -1, -1)

-- "Maybe Pitch": fixed pitch or named variable
data MPitch = Fixed Pitch | Var String
  deriving Show

toMPitch :: (Location, Maybe Pitch) -> MPitch
toMPitch (Location voice bar beat, Nothing)    = Var (toVarName (voice, bar, beat))
toMPitch (_,                       Just pitch) = Fixed pitch

toSPitch :: MPitch -> Symbolic SPitch
toSPitch (Fixed p) = return $ literal p
toSPitch (Var   s) = free s

-- Get variable pitch from SAT result; returns -1 if error.
getPitch :: SatResult -> MPitch -> Pitch
getPitch _   (Fixed p) = p
getPitch res (Var   s) = case getModelValue s res of
  Just p  -> p
  Nothing -> -1

getPitches :: SatResult -> Music MPitch -> Music Pitch
getPitches res = mapMusic (getPitch res)

data Species = First | Second

{-
-- Assumes input list has two elements; otherwise runtime error.
toPair :: [a] -> (a,a)
toPair [x , y] = (x, y)

makeCounterpoint :: Species -> [[MPitch]] -> IO SatResult
makeCounterpoint First  = makeCounterpoint1
makeCounterpoint Second = makeCounterpoint2
-}

-- Assumes both voices have the same length.
makeCounterpoint1 :: Music MPitch -> IO SatResult
makeCounterpoint1 (Music [v1, v2]) = sat $ do
  v1s <- (mapM toSPitch . flattenVoice) v1
  v2s <- (mapM toSPitch . flattenVoice) v2
  let pairs = zip v1s v2s :: [SPitchPair]
--  constrain $ sNot (repeatedNote cp)
--  constrain $ numContrary pairs .>= 4 -- 30
--  constrain $ (numLeaps cp) .== 1 -- 12
  solve $ firstSpecies pairs

-- Two part first species counterpoint.
-- Assumes input length is at least 3.
-- Counterpoint can be in either part.
firstSpecies :: forall bool pitch. (IntC bool pitch, FromInt8 pitch, SDivisible pitch) => [(pitch, pitch)] -> [bool]
firstSpecies pps =
  let start  = head pps
      middle = tail (init pps)
      end    = (last middle , last pps)

      -- Need to check both since CP might be in either voice
      scaleOk1 = map (inScale majorScale . fst) pps 
      scaleOk2 = map (inScale majorScale . snd) pps 
  
      --startOk = checkStart start
      intervalsOk = map checkInterval middle
      motionOk = checkMotion pps
      isleap = isLeap :: pitch -> bool
      --endOk = checkEnd end
      leapsOk = [numLeaps @bool (map fst pps) <= fromInt8 0]
    in intervalsOk ++ scaleOk1 ++ scaleOk2 ++ motionOk ++ leapsOk
--  in startOk ++ intervalsOk ++ motionOk ++ scaleOk1 ++ scaleOk2 ++ endOk

{-
makeCounterpoint2 :: [[MPitch]] -> IO SatResult
makeCounterpoint2 cantusFirmus = sat $ do
  cpStrong <- mkExistVars (length cantusFirmus) :: Symbolic [SPitch]
  cpWeak <- mkExistVars (length cantusFirmus) :: Symbolic [SPitch]
  let strong  = zip cantusFirmus cpStrong :: [SPitchPair]
  let cp  = zip cpStrong cpWeak :: [SPitchPair]
  let cpFlat = catPairs cp :: [SPitch]
  let second = zip cantusFirmus cp :: [(SPitch, SPitchPair)]
--  constrain $ sNot (repeatedNote cpFlat)
--  constrain $ numFalse (map (inScale majorScale) cpWeak) .<= 1
  constrain $ between cp
  solve $ secondSpecies strong
-}

between :: forall bool int. (IntC bool int, FromInt8 int, Num int) => [(int, int)] -> bool
between []                   = true
between (_     : [])         = true
between ((a,b) : (c,d) : xs) =
  ((a < b && b < c) || (a > b && b > c))
  && between ((c,d) : xs)

-- Assumes input length is at least 3
-- Cantus firmus is first pair; counterpoint is always higher.
secondSpecies :: (IntC bool pitch, FromInt8 pitch, SDivisible pitch) => [(pitch, pitch)] -> [bool]
secondSpecies pps =
  let start  = head pps
      middle = tail (init pps)
      end    = (last middle , last pps)

      scaleOk = map (inScale majorScale . snd) pps 
  
      startOk = checkStart start
      intervalsOk = map checkInterval middle
      motionOk = checkMotion pps
      endOk = checkEnd2 end
  in startOk ++ intervalsOk ++ motionOk ++ scaleOk ++ endOk

-------------------------------------------------------------------------------------

catPairs :: [(a,a)] -> [a]
catPairs []            = []
catPairs ((x,y) : xys) = x : y : catPairs xys

-------------------------------------------------------------------------------------

numContrary :: forall bool int. (IntC bool int, FromInt8 int, Num int) => [(int, int)] -> int
numContrary []                = 0
numContrary (_ : [])          = 0
numContrary (pp1 : pp2 : pps) = (ite ((contrary @bool) (pp1, pp2)) 1 0) + numContrary @bool (pp2 : pps)

repeatedNote :: forall bool int. (IntC bool int) => [int] -> bool
repeatedNote []             = false
repeatedNote (_ : [])       = false
repeatedNote (p1 : p2 : ps) = (p1 == p2) || (repeatedNote (p2 : ps))

numLeaps :: forall bool int. (IntC bool int, FromInt8 int, Num int) => [int] -> int
numLeaps []             = fromInt8 0
numLeaps (_ : [])       = fromInt8 0
numLeaps (p1 : p2 : ps) = (ite (isLeap @bool (opi (p1, p2))) 1 0) + numLeaps @bool (p2 : ps)

numTrue :: forall bool int. (IntC bool int) => [bool] -> int
numTrue xs = sum $ (map (\x -> ite x 1 0)) xs

numFalse :: forall bool int. (IntC bool int) => [bool] -> int
numFalse xs = sum $ (map (\x -> ite x 0 1)) xs
