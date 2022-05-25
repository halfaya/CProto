{-# LANGUAGE ScopedTypeVariables #-}

module Motion where

import Prelude hiding ((==), (/=), (<=), (<), (>=), (>), not, (||), (&&))

import Class
import Interval

parallel :: (IntC b a) => ((a,a) , (a,a)) -> b
parallel ((p1, q1), (p2, q2)) = opi (p1, p2) == opi (q1, q2) 

similar :: (IntC b a) => ((a,a) , (a,a)) -> b
similar pps@((p1, q1), (p2, q2)) =
  ((p1 < p2 && q1 < q2) || (p1 > p2 && q1 > q2))
  && not (parallel pps)

similarOrParallel :: (IntC b a) => ((a,a) , (a,a)) -> b
similarOrParallel ((p1, q1), (p2, q2)) =
  (p1 < p2 && q1 < q2) ||
  (p1 > p2 && q1 > q2) ||
  (p1 == p2 && q1 == q2)

contrary :: (IntC b a) => ((a,a) , (a,a)) -> b
contrary ((p1, q1), (p2, q2)) =
  (p1 < p2 && q1 > q2) ||
  (p1 > p2 && q1 < q2)

oblique :: (IntC b a) => ((a,a) , (a,a)) -> b
oblique ((p1, q1), (p2, q2)) =
  (p1 == p2 && q1 /= q2) ||
  (p1 /= p2 && q1 == q2)

checkMotionPair :: (IntC b a, FromInt8 a) => ((a,a) , (a,a)) -> b
checkMotionPair pps =
  not (isPerfect (opi (snd pps)) && similarOrParallel pps)

checkMotion :: (IntC b a, FromInt8 a) => [(a,a)] -> [b]
checkMotion []                = []
checkMotion (_ : [])          = []
checkMotion (pp1 : pp2 : pps) = checkMotionPair (pp1, pp2) : checkMotion (pp2 : pps)
