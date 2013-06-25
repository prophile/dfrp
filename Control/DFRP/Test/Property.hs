{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}

module Control.DFRP.Test.Property(propertySpec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Functor
import Data.IORef
import Data.Monoid

import Control.DFRP.EventStream
import Control.DFRP.Property

propertySpec :: Spec
propertySpec = describe "Property" $ do
  it "can keep a single value" $ do
    let prop = return (10 :: Int)
    (changes prop) `bind` (`shouldBe` 10)

  it "can combine bind and changes with watch" $ do
    let prop = return (10 :: Int)
    prop `watch` (`shouldBe` 10)

  it "can be mapped over" $ do
    let prop = return (10 :: Int)
    [2*x | x <- prop] `watch` (`shouldBe` 20)

  it "handles mapping through its Functor instance" $ do
    let prop = return (10 :: Int)
    ((* 2) <$> prop) `watch` (`shouldBe` 20)

  it "responds to changes in its input" $ do
    (stream, tx) <- newStream
    property <- scan stream
    elements <- newIORef []
    property `watch` (\x -> modifyIORef' elements (x:))
    tx $ Sum (1 :: Int)
    tx $ Sum (1 :: Int)
    results <- readIORef elements
    (map getSum results) `shouldBe` [2, 1, 0]

  it "can be created from EventStreams with latest" $ do
    (stream, tx) <- newStream
    property <- latest stream 5
    elements <- newIORef []
    property `watch` (\x -> modifyIORef' elements (x:))
    tx 10
    results <- readIORef elements
    results `shouldBe` [10, 5]

  it "can be combined" $ do
    let property1 = return (10 :: Int)
    let property2 = return (15 :: Int)
    let propertyZip = [(a, b) | a <- property1, b <- property2]
    propertyZip `watch` (`shouldBe` (10, 15))

  it "does not issue left-duplicates on source changes" $ do
    (stream, tx) <- newStream
    let propertyLeft = return (10 :: Int)
    propertyRight <- latest stream 0
    let propertyZip = [(a, b) | a <- propertyLeft, b <- propertyRight]
    elements <- newIORef []
    propertyZip `watch` (\x -> modifyIORef' elements (x:))
    tx (10 :: Int)
    results <- readIORef elements
    results `shouldBe` [(10, 10), (10, 0)]

  it "does not issue right-duplicates on source changes" $ do
    (stream, tx) <- newStream
    let propertyRight = return (10 :: Int)
    propertyLeft <- latest stream 0
    let propertyZip = [(a, b) | a <- propertyLeft, b <- propertyRight]
    elements <- newIORef []
    propertyZip `watch` (\x -> modifyIORef' elements (x:))
    tx (10 :: Int)
    results <- readIORef elements
    results `shouldBe` [(10, 10), (0, 10)]

  it "does not remember previous values under monadic join" $ do
    (streamA, txA) <- newStream
    let streamB = mzero
    propA <- latest streamA 1
    propB <- latest streamB 2
    (streamX, txX) <- newStream
    propX <- latest streamX propA
    elements <- newIORef []
    (join propX) `watch` (\x -> modifyIORef' elements (x:))
    txX propB
    txA 3
    results <- readIORef elements
    results `shouldBe` [2, 1]

