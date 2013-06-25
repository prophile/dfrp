\section{Event Streams}

As previously discussed, event streams simply form the continuation monad with
\texttt{IO ()} as the eventual type.

Luckily, the `mtl' package provides the \texttt{Cont} monad, and
we can simply build from that.

In this module, we publish:

\begin{itemize}
\item The \texttt{EventStream} type itself, a wrapper around the relevant
      \texttt{Cont} instance;
\item The typeclass instances of \texttt{EventStream},
\item A function \texttt{newStream} for creating arbitrary streams
      in the \texttt{IO} monad;
\item A function \texttt{bind} for binding streams to IO actions.
\end{itemize}

As usual with Haskell, we start with the module name and exports as listed
above.

\begin{code}
module Control.DFRP.EventStream(EventStream,
                                bind,
                                newStream,
                                fromSubscribe) where
\end{code}

We import \texttt{Control.Applicative} for the \texttt{Applicative} typeclass.

\begin{code}
import Control.Applicative
\end{code}

We import \texttt{Control.Monad.Cont} for the \texttt{Cont} monad from `mtl'.

\begin{code}
import Control.Monad.Cont
\end{code}

Finally, in the implementation, an \texttt{EventStream} `keeps track' of its
listeners. To do so, we store a list of listeners in an MVar.

\begin{code}
import Control.Concurrent.MVar
\end{code}

The \texttt{EventStream} type itself is a newtype around the continuation. We
use record syntax to also define a \texttt{getContinuation} function to get at
the underlying continuation.

\begin{code}
newtype EventStream a =
  EventStream { getContinuation :: Cont (IO ()) a }
\end{code}

The Monad instance is a simple wrapper around the \texttt{Cont} Monad instance
with the appropriate conversions between types.

\begin{code}
instance Monad EventStream where
  return = EventStream . return
  (EventStream a) >>= f =
    EventStream (a >>= (getContinuation . f))
\end{code}

The functor and applicative functor typeclasses can always be derived from a
Monad instance, and we do so in the most simple way possible here.

\begin{code}
instance Functor EventStream where
  fmap = liftM

instance Applicative EventStream where
  pure = return
  (<*>) = ap
\end{code}

The \texttt{MonadPlus} instance is more complex.

\texttt{mzero} is simple enough: listeners are ignored when being bound and no
IO action takes place.

\texttt{mplus} is more complex, but can be understood intuitivively in terms of
what it is supposed to do: we are constructing an event stream which, we given a
listener \textbf{l} to bind, binds \textbf{l} first in the left-hand stream,
then in the right-hand stream.

The ordering is arbitrary, but important -- that is to say, \texttt{mplus} is
not commutative.

Incidentally, replacing \texttt{>>} in mplus with \texttt{`mappend`} and
replacing \texttt{return ()} in \texttt{mzero} with \texttt{mempty} reveals that
the continuation monad can be made into a \texttt{MonadPlus} whenever the
eventual type forms a monoid, which in this case is the monoid $\langle \text{IO
()}, \text{>>}, \text{return ()} \rangle$.

\begin{code}
instance MonadPlus EventStream where
  mzero = EventStream $ cont $ const $ return ()
  (EventStream a) `mplus` (EventStream b) =
    EventStream $ cont $ \ l ->
      (a `runCont` l) >> (b `runCont` l)
\end{code}

The \texttt{MonadCont} instance for \texttt{EventStream} is rather simply
defined in terms of the \texttt{MonadCont} instance for the underlying
continuation monad, the only difference being the type conversions.

\begin{code}
instance MonadCont EventStream where
  callCC f = EventStream $ callCC $ \k ->
               getContinuation $ f (EventStream . k)
\end{code}

The \texttt{bind} function is trivial -- it is simply \texttt{runCont} mapped to
the appropriate type, which itself is just the gateway to the innards of the
continuation monad in much the same manner as \texttt{getContinuation} is used
in \texttt{EventStream}.

\begin{code}
bind :: EventStream a -> (a -> IO ()) -> IO ()
bind (EventStream c) = runCont c
\end{code}

Perhaps the most complex piece of code in this module is \texttt{newStream}
which is also probably the least -- from a formal point of view -- interesting.

It creates a \texttt{MVar} to store the list of listeners, defines the methods
to both add listeners and transmit to them, and returns them.

\begin{code}
newStream :: IO (EventStream a, a -> IO ())
newStream = do
  listeners <- newMVar []
  let addListener l = modifyMVar_ listeners $ \ls ->
        return $ l:ls
  let tx x = withMVar listeners (\r -> forM_ r ($ x))
  return (EventStream (cont addListener), tx)
\end{code}

Undocumented!

\begin{code}
fromSubscribe :: ((a -> IO ()) -> IO ()) -> EventStream a
fromSubscribe = EventStream . cont
\end{code}

