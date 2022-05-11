{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Class where

import Data.SBV

class BoolC b where
  not   :: b -> b
  (^&&) :: b -> b -> b
  (^||) :: b -> b -> b

instance BoolC Bool where
  not   = Prelude.not
  (^&&) = (&&)
  (^||) = (||)

instance BoolC SBool where
  not   = sNot
  (^&&) = (.&&)
  (^||) = (.||)

class (BoolC b) => IntC b a where
  (^==) :: a -> a -> b
  (^/=) :: a -> a -> b
  (^<)  :: a -> a -> b
  (^<=) :: a -> a -> b
  (^>)  :: a -> a -> b
  (^>=) :: a -> a -> b

instance IntC Bool Int8 where
  (^==) = (==)
  (^/=) = (/=)
  (^<)  = (<)
  (^<=) = (<=)
  (^>)  = (>)
  (^>=) = (>=)

instance IntC SBool SInt8 where
  (^==) = (.==)
  (^/=) = (./=)
  (^<)  = (.<)
  (^<=) = (.<=)
  (^>)  = (.>)
  (^>=) = (.>=)

class CastC a where
  cast :: Int8 -> a

instance CastC Int8 where
  cast = id

instance CastC SInt8 where
  cast = literal
  
  
