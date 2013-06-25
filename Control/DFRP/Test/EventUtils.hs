{-# LANGUAGE MonadComprehensions #-}

module Control.DFRP.Test.EventUtils(eventUtilsSpec) where

import Test.Hspec
import Test.QuickCheck

import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Data.Functor
import Data.IORef

import Control.DFRP.EventStream
import Control.DFRP.EventUtils

eventUtilsSpec :: Spec
eventUtilsSpec = describe "EventUtils" $ do
  describe "fromList" $ do
    it "fires an event for every item in a list" $ do
      let source = fromList ([1, 2, 3] :: [Int])
      elements <- newIORef []
      source `bind` (\x -> modifyIORef' elements (x:))
      results <- readIORef elements
      results `shouldBe` [3, 2, 1]

    it "correctly fires no event given the empty list" $ do
      let source = fromList []
      received <- newIORef False
      source `bind` (const $ writeIORef received True)
      result <- readIORef received
      result `shouldBe` False

  describe "flattenStream" $ do
    it "flattens arrays into distinct events" $ do
      let source = return ([1, 2, 3] :: [Int])
      let source' = flattenStream source
      elements <- newIORef []
      source' `bind` (\x -> modifyIORef' elements (x:))
      results <- readIORef elements
      results `shouldBe` [3, 2, 1]

