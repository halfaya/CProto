{-# LANGUAGE ScopedTypeVariables #-}

module Counterpoint where

import Data.List.Split (splitOn)
import Data.Map (toList)
import Data.SBV
import Data.SBV.Internals (CV)
import GHC.Exts (sortWith)
import Prelude hiding ((==), (/=), (<=), (<), (>=), (>), not, (||), (&&))

import Boundary
import Class
import Interval
import Pitch
import Interval
import Motion

toVarName :: Int -> Int -> String
toVarName voice position = show voice ++ "_" ++ show position

fromVarName :: String -> (Int, Int)
fromVarName s = case splitOn "_" s of
  [v , p] -> (read v , read p)
  _       -> (-1, -1)

-- "Maybe Pitch": fixed pitch or named variable
data MPitch = Fixed Pitch | Var String
  deriving Show

toMPitch :: Int -> Int -> Maybe Pitch -> MPitch
toMPitch position voice Nothing      = Var (toVarName position voice)
toMPitch _        _     (Just pitch) = Fixed pitch

toMPitches1 :: Int -> [Maybe Pitch] -> [MPitch]
toMPitches1 position ps = map (uncurry (toMPitch position)) (zip [0..] ps)

toMPitches :: [[Maybe Pitch]] -> [[MPitch]]
toMPitches pss = map (uncurry toMPitches1) (zip [0..] pss)

toSPitch :: MPitch -> Symbolic SPitch
toSPitch (Fixed p) = return $ literal p
toSPitch (Var   s) = free s

toSPitches1 :: [MPitch] -> Symbolic [SPitch]
toSPitches1 = mapM toSPitch

toSPitches :: [[MPitch]] -> Symbolic [[SPitch]]
toSPitches = mapM toSPitches1

-- Get variable pitch from SAT result; returns -1 if error.
getPitch :: SatResult -> MPitch -> Pitch
getPitch _   (Fixed p) = p
getPitch res (Var   s) = case getModelValue s res of
  Just p  -> p
  Nothing -> -1

getPitches :: SatResult -> [[MPitch]] -> [[Pitch]]
getPitches res = map (map (getPitch res))

-- Assumes input list has two elements; otherwise runtime error.
toPair :: [a] -> (a,a)
toPair [x , y] = (x, y)

data Species = First | Second

{-
makeCounterpoint :: Species -> [Pitch] -> IO SatResult
makeCounterpoint First  = makeCounterpoint1
makeCounterpoint Second = makeCounterpoint2
-}

makeCounterpoint1 :: [[MPitch]] -> IO SatResult
makeCounterpoint1 music = sat $ do
  m1 <- toSPitches music :: Symbolic [[SPitch]]
  let pairs = map toPair m1 :: [SPitchPair]
--  constrain $ sNot (repeatedNote cp)
--  constrain $ numContrary pairs .>= 4 -- 30
--  constrain $ (numLeaps cp) .== 1 -- 12
  solve $ firstSpecies pairs

-- Assumes input length is at least 3
-- Cantus firmus is first pair; counterpoint is always higher.
firstSpecies :: (IntC bool pitch, FromInt8 pitch, SDivisible pitch) => [(pitch, pitch)] -> [bool]
firstSpecies pps =
  let start  = head pps
      middle = tail (init pps)
      end    = (last middle , last pps)

      -- Need to check both since CP might be in either voice
      scaleOk1 = map (inScale majorScale . fst) pps 
      scaleOk2 = map (inScale majorScale . snd) pps 
  
      --startOk = checkStart start
      intervalsOk = map checkInterval4 middle
      motionOk = checkMotion pps
      --endOk = checkEnd end
    in intervalsOk ++ scaleOk1 ++ scaleOk2 ++ motionOk
--  in startOk ++ intervalsOk ++ motionOk ++ scaleOk1 ++ scaleOk2 ++ endOk 

makeCounterpoint2 :: [SPitch] -> IO SatResult
makeCounterpoint2 cantusFirmus = sat $ do
  cpStrong <- mkExistVars (length cantusFirmus) :: Symbolic [SPitch]
  cpWeak <- mkExistVars (length cantusFirmus) :: Symbolic [SPitch]
  let strong  = zip cantusFirmus cpStrong :: [SPitchPair]
  let cp  = zip cpStrong cpWeak :: [SPitchPair]
  let cpFlat = catPairs cp :: [SPitch]
  let second = zip cantusFirmus cp :: [(SPitch, SPitchPair)]
  constrain $ sNot (repeatedNote cpFlat)
  constrain $ numFalse (map (inScale majorScale) cpWeak) .<= 1
  constrain $ between cp
  solve $ secondSpecies strong

between :: [SPitchPair] -> SBool
between []                   = sTrue
between (_     : [])         = sTrue
between ((a,b) : (c,d) : xs) =
  ((a .< b .&& b .< c) .|| (a .> b .&& b .> c))
  .&& between ((c,d) : xs)

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

{-
cvsToInt8s :: [CV] -> [Int8]
cvsToInt8s [] = []
cvsToInt8s cvs = case parseCVs cvs of
  Nothing         -> []
  Just (w , cvs') -> w : cvsToInt8s cvs'

-- Expects variables to be of the form s0, s1, etc., so remove the "s" and sort via integer value.
getPitches1 :: SatResult -> [Pitch]
getPitches1 res = map fromIntegral (cvsToInt8s $ map snd (sortWith (r . tail . fst) (toList (getModelDictionary res))))
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

getPitches :: Species -> SatResult -> [Pitch]
getPitches First  = getPitches1
getPitches Second = getPitches2
-}

-------------------------------------------------------------------------------------

numContrary :: [SPitchPair] -> SInteger
numContrary []                = 0
numContrary (_ : [])          = 0
numContrary (pp1 : pp2 : pps) = (ite (contrary (pp1, pp2)) 1 0) + numContrary (pp2 : pps)

repeatedNote :: [SPitch] -> SBool
repeatedNote []             = sFalse
repeatedNote (_ : [])       = sFalse
repeatedNote (p1 : p2 : ps) = ite (p1 .== p2) sTrue (repeatedNote (p2 : ps))

upi :: SPitchPair -> SInt8
upi (p1 , p2) = ite (p1 .< p2) (p2 - p1) (p1 - p2)

numLeaps :: [SPitch] -> SInteger
numLeaps []             = 0
numLeaps (_ : [])       = 0
numLeaps (p1 : p2 : ps) = (ite (isLeap (upi (p1, p2))) 1 0) + (numLeaps (p2 : ps))

numTrue :: [SBool] -> SInteger
numTrue xs = sum $ (map (\x -> ite x 1 0)) xs

numFalse :: [SBool] -> SInteger
numFalse xs = sum $ (map (\x -> ite x 0 1)) xs
