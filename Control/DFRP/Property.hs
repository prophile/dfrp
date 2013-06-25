{-# LANGUAGE MonadComprehensions #-}

module Control.DFRP.Property(Property,
                             scan,
                             latest,
                             changes,
                             watch) where

import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.Functor
import Control.Concurrent.MVar
import Control.Monad.Cont
import Control.DFRP.EventStream
import Data.IORef
import Data.Unique
import System.IO.Unsafe(unsafePerformIO)

data Property a = Property (Cont (IO ()) a)

instance Functor Property where
  fmap f (Property a) =
    Property $ cont $ \ l -> a `runCont` (l . f)

joinProperty :: Property (Property a) -> Property a
joinProperty p = Property $ cont $ \ l -> do
    initialToken <- newUnique
    token <- newIORef initialToken
    let handleSubproperty ps = do
          activeToken <- newUnique
          writeIORef token activeToken
          ps `watch` (\value -> do
            expectedToken <- readIORef token
            unless (expectedToken /= activeToken) $ l value)
    p `watch` handleSubproperty

instance Monad Property where
  return = Property . return
  p >>= f = joinProperty (f <$> p)

scan :: (Monoid m) => EventStream m -> Property m
scan stream = unsafePerformIO $ do
    value <- newIORef mempty
    listeners <- newMVar []
    let tx value = withMVar listeners $ \ receivers ->
          forM_ receivers ($ value)
    let broadcast = readIORef value >>= tx
    let update x = modifyIORef' value (`mappend` x)
    let receive x = withMVar listeners (\_ -> update x) >> broadcast
    let addListener l = do
          modifyMVar_ listeners (return . (l :))
          initialValue <- readIORef value
          l initialValue
    stream `bind` receive
    return $ Property $ cont addListener
{-# NOINLINE scan #-}

latest :: EventStream a -> a -> Property a
latest strm dfl = (fromMaybe dfl . getLast) <$> baseProperty
    where baseProperty = scan eventStream
          eventStream = (Last . Just) <$> strm

changes :: Property a -> EventStream a
changes (Property strm) = fromSubscribe (runCont strm)

watch :: Property a -> (a -> IO ()) -> IO ()
watch p = bind $ changes p

