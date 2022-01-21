{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dddf.Internal.TypeUtil where

import Data.Symbol.Ascii
import GHC.TypeLits

type And :: Bool -> Bool -> Bool
type family And a b

type instance And True True = True
type instance And True False = False
type instance And False True = False
type instance And False False = False

type family Length xs where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family IsInRange (min :: Nat) (max :: Nat) (s :: Symbol) where
    IsInRange min max s = And (min <=? Length (ToList s)) (Length (ToList s) <=? max)
