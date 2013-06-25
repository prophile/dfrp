{-# LANGUAGE MonadComprehensions #-}

module Control.DFRP.Property where

import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.Functor
import Control.Concurrent.MVar
import Control.Monad.Cont
import Control.DFRP.EventStream
import Data.IORef

data Property a = Property {getContinuation :: Cont (IO ()) a}

instance Monad Property where
  return = Property . return
  (Property a) >>= f =
    Property (a >>= (getContinuation . f))

instance Functor Property where
  fmap = liftM

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

