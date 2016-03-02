{-# LANGUAGE GADTs #-}

module Data.TextIO.IO
  ( runTextIO
  ) where

import           Control.Monad.Catch
import           Control.Monad.Operational
import qualified Data.Text.IO              as T
import           Data.TextIO.Internal

runTextIO :: TextIO a -> IO a
runTextIO tio = eval $ view tio
  where
    eval :: ProgramView TextIOOpe a -> IO a
    eval (PutText x :>>= is) = do
      T.putStr x
      runTextIO $ is ()
    eval (GetCh     :>>= is) = do
      ch <- getChar
      runTextIO $ is ch
    eval (Throw   x :>>= is) = do
      throwM x
      runTextIO $ is ()
    eval (Return x)          = return x
