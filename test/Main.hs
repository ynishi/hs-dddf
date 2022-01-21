{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.DeepSeq (rnf)
import Data.Proxy
import GHC.TypeLits
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@=?), (@?=))
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck ((.&&.), (===), (==>))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty
import Prelude hiding (id, (.))

import Dddf
import Dddf.Internal

main :: IO ()
main =
    Tasty.defaultMainWithIngredients
        [ Tasty.consoleTestReporter
        , Tasty.listingTests
        ]
        (Tasty.localOption (QC.QuickCheckTests 100) tests)

tests :: Tasty.TestTree
tests =
    Tasty.testGroup
        "root"
        [ testFixeds
        ]

testFixeds :: Tasty.TestTree
testFixeds =
    Tasty.testGroup
        "Fixed"
        [ testFixed (Proxy :: Proxy 1, Proxy :: Proxy 2)
        ]

testFixed ::
    forall min max.
    (KnownNat min, KnownNat max) =>
    (Proxy min, Proxy max) ->
    Tasty.TestTree
testFixed (pmin, pmax) =
    Tasty.testGroup
        ("Fixed " ++ show (natVal pmin) ++ ", " ++ show (natVal pmax))
        [ QC.testProperty "read" $
            QC.forAll QC.arbitrary $ \x ->
                (Fixed x :: FixedT pmin pmax) === read (show (Fixed x :: FixedT pmin pmin))
        ]
