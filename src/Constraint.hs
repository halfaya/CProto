{-# LANGUAGE ScopedTypeVariables #-}

module Constraint where

import Pitch
import Interval

isInterval :: PI -> [PI] -> Bool
isInterval = elem
