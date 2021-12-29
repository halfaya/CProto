{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Map (toList)
import Data.SBV
import Data.SBV.Internals (CV)
import GHC.Exts (sortWith)

import Pitch
import Interval
import Yamanote

test :: IO SatResult
test = sat $ do
  ps <- mkExistVars (length cantusFirmus)
  let xs = zip cantusFirmus ps :: [(Pitch, Pitch)]
  solve $ map (isPerfect . upi) xs

cvsToWord8s :: [CV] -> [Word8]
cvsToWord8s [] = []
cvsToWord8s cvs = case parseCVs cvs of
  Nothing         -> []
  Just (w , cvs') -> w : cvsToWord8s cvs'

-- Expects variables to be of the form s0, s1, etc., so remove the "s" and sort via integer value
getPitches :: SatResult -> [Word8]
getPitches res = cvsToWord8s $ map snd (sortWith (r . tail . fst) (toList (getModelDictionary res)))
  where r :: String -> Int
        r = read

main :: IO ()
main = do
  res <- test
  putStrLn $ show $ getPitches res
