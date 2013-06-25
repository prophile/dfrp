{-# LANGUAGE MonadComprehensions #-}

import Test.Hspec

import Control.DFRP.Test.EventStream
import Control.DFRP.Test.EventUtils
import Control.DFRP.Test.Property

main :: IO ()
main = hspec $ do
  eventStreamSpec
  propertySpec
  eventUtilsSpec

