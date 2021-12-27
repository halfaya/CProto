{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.SBV

main :: IO ()
main = do
  res <- prove $ \(x :: SWord8) -> x `shiftL` 2 .== 3 * x
  putStrLn $ show res
