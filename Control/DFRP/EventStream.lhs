\section{Event Streams}

\begin{code}
module Control.DFRP.EventStream(EventStream, bind, newStream) where

import Control.Applicative
import Control.Monad.Cont
import Control.Concurrent.MVar

newtype EventStream a = EventStream { getContinuation :: Cont (IO ()) a }

instance Monad EventStream where
  return = EventStream . return
  (EventStream a) >>= f = EventStream (a >>= (getContinuation . f))

instance Functor EventStream where
  fmap = liftM

instance Applicative EventStream where
  pure = return
  (<*>) = ap

instance MonadPlus EventStream where
  mzero = EventStream $ cont $ const $ return ()
  (EventStream a) `mplus` (EventStream b) =
    EventStream $ cont $ \ l -> (a `runCont` l) >> (b `runCont` l)

bind :: EventStream a -> (a -> IO ()) -> IO ()
bind (EventStream c) = runCont c

newStream :: IO (EventStream a, a -> IO ())
newStream = do
  listeners <- newMVar []
  let addListener l = modifyMVar_ listeners (\ls -> return $ l:ls)
  let tx x = withMVar listeners (\r -> forM_ r ($ x))
  return (EventStream (cont addListener), tx)
\end{code}

