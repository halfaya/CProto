{-# LANGUAGE ScopedTypeVariables #-}

module Beethoven where

import Pitch
import Music

-- Example 146 (page 29, number 2) in Beethoven Werke XIII
beethoven146 :: [PitchPair]
beethoven146 =
  [(g 5 , c 6) ,
   (c 6 , e 6) ,
   (b 5 , g 6) ,
   (a 5 , f 6) ,
   (g 5 , e 6) ,
   (f 5 , c 6) ,
   (a 5 , a 6) ,
   (c 6 , f 6) ,
   (b 5 , g 6) ,
   (g 5 , e 6) ,
   (g 5 , d 6) ,
   (g 5 , c 6) ]

beethoven146a :: FirstSpecies2 (Maybe Pitch)
beethoven146a = FirstSpecies2
  [(Just (g 5) , Just (c 6)) ,
   (Just (c 6) , Just (e 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (a 5) , Just (f 6)) ,
   (Just (g 5) , Just (e 6)) ,
   (Just (f 5) , Just (c 6)) ,
   (Just (a 5) , Just (a 6)) ,
   (Just (c 6) , Just (f 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (g 5) , Just (e 6)) ,
   (Just (g 5) , Just (d 6)) ,
   (Just (g 5) , Just (c 6)) ]

-- Haydn's correction
beethoven146h :: FirstSpecies2 (Maybe Pitch)
beethoven146h = FirstSpecies2
  [(Just (g 5) , Just (c 6)) ,
   (Just (c 6) , Just (e 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (a 5) , Just (f 6)) ,
   (Just (g 5) , Just (e 6)) ,
   (Just (a 5) , Just (c 6)) , -- f5 -> a5
   (Just (a 5) , Just (a 6)) ,
   (Just (c 6) , Just (f 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (g 5) , Just (e 6)) ,
   (Just (g 5) , Just (d 6)) ,
   (Just (g 5) , Just (c 6)) ]

-- Eliminating one note at mistake
beethoven146_1 :: FirstSpecies2 (Maybe Pitch)
beethoven146_1 = FirstSpecies2
  [(Just (g 5) , Just (c 6)) ,
   (Just (c 6) , Just (e 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (a 5) , Just (f 6)) ,
   (Just (g 5) , Just (e 6)) ,
   (Nothing    , Just (c 6)) ,
   (Just (a 5) , Just (a 6)) ,
   (Just (c 6) , Just (f 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (g 5) , Just (e 6)) ,
   (Just (g 5) , Just (d 6)) ,
   (Just (g 5) , Just (c 6)) ]

-- Eliminating three notes at mistake
beethoven146_3 :: FirstSpecies2 (Maybe Pitch)
beethoven146_3 = FirstSpecies2
  [(Just (g 5) , Just (c 6)) ,
   (Just (c 6) , Just (e 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (a 5) , Just (f 6)) ,
   (Nothing    , Just (e 6)) ,
   (Nothing    , Just (c 6)) ,
   (Nothing    , Just (a 6)) ,
   (Just (c 6) , Just (f 6)) ,
   (Just (b 5) , Just (g 6)) ,
   (Just (g 5) , Just (e 6)) ,
   (Just (g 5) , Just (d 6)) ,
   (Just (g 5) , Just (c 6)) ]

-- Cantus firmus only, plus start and end of counterpoint
beethoven146cf :: FirstSpecies2 (Maybe Pitch)
beethoven146cf = FirstSpecies2
  [(Just (g 5) , Just (c 6)) ,
   (Nothing    , Just (e 6)) ,
   (Nothing    , Just (g 6)) ,
   (Nothing    , Just (f 6)) ,
   (Nothing    , Just (e 6)) ,
   (Nothing    , Just (c 6)) ,
   (Nothing    , Just (a 6)) ,
   (Nothing    , Just (f 6)) ,
   (Nothing    , Just (g 6)) ,
   (Nothing    , Just (e 6)) ,
   (Nothing    , Just (d 6)) ,
   (Just (g 5) , Just (c 6)) ]
