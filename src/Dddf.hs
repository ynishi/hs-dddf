{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dddf (
    FixedConsException,
    FixedT (..),
    FixedInputT,
    mkFixed,
    fixed,
) where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy
import Data.Symbol.Ascii
import GHC.TypeLits
import Money

import Dddf.Internal

newtype FixedConsException = FixedConsException String deriving (Show)
instance Exception FixedConsException
newtype FixedT (min :: Nat) (max :: Nat) = Fixed String deriving (Eq, Ord, Read, Show)

fixed :: forall m min max s. (KnownNat min, KnownNat max, MonadThrow m) => String -> m (FixedT min max)
fixed s
    | min' <= l && l <= max' = return $ Fixed s
    | l < min' = failed "too small"
    | max' < l = failed "too big"
    | otherwise = failed "something wrong"
  where
    l = length s
    min' = fromIntegral (natVal (Proxy @min))
    max' = fromIntegral (natVal (Proxy @max))
    failed s = throwM $ FixedConsException s

data FixedInputT n = FixedI deriving (Show)

mkFixed :: forall min max s. (KnownNat min, KnownNat max, KnownSymbol s, IsInRange min max s ~ True) => FixedInputT s -> FixedT min max
mkFixed FixedI = Fixed $ symbolVal (Proxy @s)
