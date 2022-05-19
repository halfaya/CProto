{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Class where

import qualified Prelude as P
import Data.SBV

-- A boolean-like class
class BoolC b where
  not  :: b -> b
  (&&) :: b -> b -> b
  (||) :: b -> b -> b

infixr 3 &&
infixr 2 ||

instance BoolC P.Bool where
  not   = P.not
  (&&) = (P.&&)
  (||) = (P.||)

instance BoolC SBool where
  not   = sNot
  (&&) = (.&&)
  (||) = (.||)

-- An integer-like class depending on a boolean-like class
class (P.Num a, BoolC b) => IntC b a where
  (==) :: a -> a -> b
  (/=) :: a -> a -> b
  (<)  :: a -> a -> b
  (<=) :: a -> a -> b
  (>)  :: a -> a -> b
  (>=) :: a -> a -> b

infixr 4 ==, /=, <, <=, >, >=

instance IntC P.Bool Int8 where
  (==) = (P.==)
  (/=) = (P./=)
  (<)  = (P.<)
  (<=) = (P.<=)
  (>)  = (P.>)
  (>=) = (P.>=)

instance IntC SBool SInt8 where
  (==) = (.==)
  (/=) = (./=)
  (<)  = (.<)
  (<=) = (.<=)
  (>)  = (.>)
  (>=) = (.>=)

-- A type that can be cast to from Int8
class FromInt8 a where
  fromInt8 :: Int8 -> a

instance FromInt8 Int8 where
  fromInt8 = P.id

instance FromInt8 SInt8 where
  fromInt8 = literal
