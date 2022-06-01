{-# LANGUAGE ScopedTypeVariables #-}

module Music where

import Data.List (intercalate)

newtype Beat a  = Beat a

newtype Bar a   = Bar [Beat a]

newtype Voice a = Voice [Bar a]

newtype Music a = Music [Voice a]

instance (Show a) => Show (Bar a) where
  show (Bar xs) = show (map (\(Beat x) -> x) xs)

instance (Show a) => Show (Voice a) where
  show (Voice xs) = intercalate " | " (map show xs)

instance (Show a) => Show (Music a) where
  show (Music xs) = intercalate " ||\n" (map show xs)

-- voice, bar, beat
data Location = Location Int Int Int

instance Show Location where
  show (Location voice bar beat) = "Voice " ++ show voice ++ " bar " ++ show bar ++ " beat " ++ show beat

--instance Range1 

mapBeat :: (a -> b) -> Beat a -> Beat b
mapBeat f (Beat x) = Beat (f x)

mapBar :: (a -> b) -> Bar a -> Bar b
mapBar f (Bar xs) = Bar (map (mapBeat f) xs)

mapVoice :: (a -> b) -> Voice a -> Voice b
mapVoice f (Voice xs) = Voice (map (mapBar f) xs)

mapMusic :: (a -> b) -> Music a -> Music b
mapMusic f (Music xs) = Music (map (mapVoice f) xs)

-- count beats per bar
indexBar :: Int -> Int -> Bar a -> Bar (Location, a)
indexBar voice bar (Bar xs) = Bar (map (\(beat, Beat x) -> Beat (Location voice bar beat, x)) (zip [0..] xs))

-- count bars per voice and beats per bar
indexVoice :: Int -> Voice a -> Voice (Location, a)
indexVoice voice (Voice xs) = Voice (map (\(bar, x) -> indexBar voice bar x) (zip [0..] xs))

-- count voices, bars per voice and beats per bar
indexMusic :: Music a -> Music (Location, a)
indexMusic (Music xs) = Music (map (\(voice, x) -> indexVoice voice x) (zip [0..] xs))

flattenBar :: Bar a -> [a]
flattenBar (Bar xs) = map (\(Beat x) -> x) xs

flattenVoice :: Voice a -> [a]
flattenVoice (Voice xs) = concat (map flattenBar xs)

flattenMusic :: Music a -> [[a]]
flattenMusic (Music xs) = map flattenVoice xs

-- one beat per bar
b1 :: a -> Bar a
b1 x = Bar [Beat x]

-- two beats per bar
b2 :: (a, a) -> Bar a
b2 (x, y) = Bar [Beat x, Beat y]

-- Two part first species, a sequence of pairs.
newtype FirstSpecies2 a = FirstSpecies2 [(a,a)]

firstSpecies2toMusic :: FirstSpecies2 a -> Music a
firstSpecies2toMusic (FirstSpecies2 xs) =
  let v1 = map (b1 . fst) xs
      v2 = map (b1 . snd) xs
  in Music [Voice v1, Voice v2]
