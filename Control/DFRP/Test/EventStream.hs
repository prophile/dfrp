{-# LANGUAGE MonadComprehensions #-}

module Control.DFRP.Test.EventStream(eventStreamSpec) where

import Test.Hspec
import Test.QuickCheck

import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Data.Functor
import Data.IORef

import Control.DFRP.EventStream

eventStreamSpec :: Spec
eventStreamSpec = describe "EventStream" $ do
  it "can return single values" $ do
    let stream = return (10 :: Int)
    stream `bind` (`shouldBe` 10)

  it "has a pure which returns single values" $ do
    let stream = pure (10 :: Int)
    stream `bind` (`shouldBe` 10)

  it "has a straightforward Applicative instance" $ do
    let streamF = pure (* 2)
    let streamV = pure (10 :: Int)
    (streamF <*> streamV) `bind` (`shouldBe` 20)

  it "handles mapping through its Monad instance" $ do
    let stream = return (10 :: Int)
    [2*x | x <- stream] `bind` (`shouldBe` 20)

  it "handles mapping through its Functor instance" $ do
    let stream = return (10 :: Int)
    ((* 2) <$> stream) `bind` (`shouldBe` 20)

  it "allows creation of streams in the IO monad" $ do
    (stream, tx) <- newStream
    stream `bind` (`shouldBe` 10)
    tx (10 :: Int)

  it "allows multiple transmissions" $ do
    (stream, tx) <- newStream
    elements <- newIORef []
    stream `bind` (\x -> modifyIORef' elements (x:))
    tx (10 :: Int)
    tx (15 :: Int)
    results <- readIORef elements
    results `shouldBe` [15, 10]

  it "does not rebroadcast to old listeners" $ do
    (stream, tx) <- newStream
    tx (10 :: Int)
    stream `bind` (`shouldBe` 15)
    tx (15 :: Int)

  it "can filter streams" $ do
    (stream, tx) <- newStream
    elements <- newIORef []
    [x | x <- stream, x < 12] `bind` (\x -> modifyIORef' elements (x:))
    tx (10 :: Int)
    tx (15 :: Int)
    results <- readIORef elements
    results `shouldBe` [10]

  it "allows stream merging" $ do
    (streamA, txA) <- newStream
    (streamB, txB) <- newStream
    let totalStream = streamA `mplus` streamB
    elements <- newIORef []
    totalStream `bind` (\x -> modifyIORef' elements (x:))
    txA (10 :: Int)
    txB (15 :: Int)
    results <- readIORef elements
    results `shouldSatisfy` (10 `elem`)
    results `shouldSatisfy` (15 `elem`)

  it "has left-to-right ordering of merged elements" $ do
    let streamA = return (10 :: Int)
    let streamB = return (15 :: Int)
    let totalStream = streamA `mplus` streamB
    elements <- newIORef []
    totalStream `bind` (\x -> modifyIORef' elements (x:))
    results <- readIORef elements
    results `shouldBe` [15, 10]

  it "has the usual 'escape mechanism' via callCC" $ do
    let stream = mzero :: EventStream Int
    let calledStream = callCC $ \k -> do
          k 10
          return 15
    calledStream `bind` (`shouldBe` 10)

  it "works correctly without a k-call with callCC" $ do
    let stream = mzero :: EventStream Int
    let calledStream = callCC $ \k -> return 15
    calledStream `bind` (`shouldBe` 15)

