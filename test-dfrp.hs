{-# LANGUAGE MonadComprehensions #-}

import Test.Hspec

import Control.DFRP.Test.EventStream

main :: IO ()
main = hspec $ do
  eventStreamSpec
