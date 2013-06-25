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

data Property a = Property (Cont (IO ()) a)

instance Functor Property where
  fmap f (Property a) =
    Property $ cont $ \ l -> a `runCont` (l . f)

joinProperty :: Property (Property a) -> Property a
--joinProperty (Property b) = let c = runCont b in
--    Property $ cont $ \ d -> c $ \ (Property f) -> let g = runCont f in g d
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

scan :: (Monoid m) => EventStream m -> IO (Property m)
scan stream = do
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

latest :: EventStream a -> a -> IO (Property a)
latest strm dfl = do
    let eventStream = (Last . Just) <$> strm
    baseProperty <- scan eventStream
    return $ (fromMaybe dfl . getLast) <$> baseProperty

changes :: Property a -> EventStream a
changes (Property strm) = fromSubscribe (runCont strm)

watch :: Property a -> (a -> IO ()) -> IO ()
watch p = bind $ changes p

