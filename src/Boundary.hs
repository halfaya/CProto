{-# LANGUAGE ScopedTypeVariables #-}

module Boundary where

import Prelude hiding ((==), (/=), (<=), (<), (>=), (>), not, (||), (&&))

import Class
import Interval
import Pitch

-- Counterpoint must be a unison, perfect 5th or perfect octave above cantus firmus.
checkStart :: (IntC bool pitch, FromInt8 pitch) => (pitch, pitch) -> [bool]
checkStart pp = is158 (opi pp) : []

-- Note that the cantus firmus must end by a half step up or full step down.
-- If this is not satisfied the generation of counterpoint will fail.
checkEnd :: (IntC bool pitch, FromInt8 pitch) => ((pitch, pitch), (pitch,pitch)) -> [bool]
checkEnd ((p1, q1), (p2, q2)) =
  let f = fromInt8 . iv
      c1 = opi (p2, q2) == f Per8 -- counterpoint ends a perfect octave above the cantus firmus
      c2 = opi (p1, p2) == f Min2 && opi (q2, q1) == f Maj2 -- cf half step up and cp full step down
      c3 = opi (p2, p1) == f Maj2 && opi (q1, q2) == f Min2 -- cf full step up and cp half step down
  in c1 : (c2 || c3): []

-- Note that the cantus firmus must end by a half step up or full step down.
-- If this is not satisfied the generation of counterpoint will fail.
checkEnd2 :: (IntC bool pitch, FromInt8 pitch) => ((pitch, pitch), (pitch,pitch)) -> [bool]
checkEnd2 ((p1, q1), (p2, q2)) =
  let f = fromInt8 . iv
      c1 = opi (p2, q2) == f Per8 -- counterpoint ends a perfect octave above the cantus firmus
      c2 = opi (p1, p2) == f Min3 && opi (q2, q1) == f Maj2 -- cf half step up and cp full step down
      c3 = opi (p2, p1) == f Maj2 && opi (q1, q2) == f Min3 -- cf full step up and cp half step down
  in c1 : (c2 || c3): []

