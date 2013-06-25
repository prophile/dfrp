{-# LANGUAGE MonadComprehensions #-}

module Control.DFRP.EventUtils where

import Control.Monad

import Control.DFRP.EventStream

fromList :: [a] -> EventStream a
fromList = msum . map return

flattenStream :: EventStream [a] -> EventStream a
flattenStream = (>>= fromList)

