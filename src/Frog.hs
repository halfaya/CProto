{-# LANGUAGE ScopedTypeVariables #-}

module Frog where

import Data.SBV

import Pitch

-- Frog melody
frog :: [Pitch]
frog =
  c 4 : d 4 : e 4 : f 4 : e 4 : d 4 : c 4 : []
