{-# LANGUAGE ScopedTypeVariables #-}

module Yamanote where

import Data.SBV

import Pitch

-- Yamanoto melody transposed down an octave and with an additional d6 before the end.
yamanote :: [Pitch]
yamanote =
  g 5 : g 4  : a 4 : g 4 : c 5 : g 4 : e 5 : g 4 :
  g 5 : g 4  : a 4 : g 4 : c 5 : g 4 : e 5 : g 4 :
  g 5 : g 4  : a 4 : g 4 : b 4 : g 4 : d 5 : g 4 :
  g 5 : g 4  : a 4 : g 4 : b 4 : g 4 : d 5 : g 4 :
  g 4 : e 4  : g 4 : c 5 : c 5 : c 5 : e 5 : g 5 :
  d 6 : c 6  : []
