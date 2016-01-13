{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module LibLSSP.Streams.Types
  ( LoggerLevel (..)
  , runTextIOConsole
  , hello
  , putText
  , getText
  , logText
  , isEOF
  , stateIOM
  , sourceIOM
  , sinkIOM
  , coIOM
  , TextIOM
  ) where

import           Control.Lens
import           Control.Monad         (replicateM)
import           Control.Monad.Catch   ()
import           Control.Monad.Free
import           Control.Monad.Free.TH (makeFree)
import           Control.Monad.State
import qualified Data.Conduit          as C
import qualified Data.Conduit.List     as CL
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Prelude               hiding (getChar, putStr, putStrLn)
import qualified System.IO             as SIO (getChar, isEOF, putStr)

data LoggerLevel
  = DEBUG
  | INFO
  | NOTICE
  | WARNING
  | ERROR
  | ALERT
  deriving ( Eq, Ord, Show )

data TextIO a
  = GetChar (Char -> a)
  | PutText T.Text a
  | LogText LoggerLevel T.Text a
  | IsEOF (Bool -> a)
--  | Halt Int
  deriving ( Functor )

makeFree ''TextIO

type TextIOM = Free TextIO

getText :: (MonadFree TextIO m) => Int -> m T.Text
getText n = T.pack <$> replicateM n getChar

errorMessage :: Int -> String
errorMessage 0 = "ok"
errorMessage _ = "error!"

runTextIOConsole :: TextIOM a -> IO a
runTextIOConsole = iterM run
  where
    run :: TextIO (IO a) -> IO a
    run tio = case tio of
      --Halt x -> throwM $ errorMessage x
      LogText l s rest -> do
        SIO.putStr $ "[" ++ show l ++ "]: "
        TIO.putStrLn s
        rest
      IsEOF f -> SIO.isEOF >>= f
      GetChar f -> SIO.getChar >>= f
      PutText s rest -> TIO.putStr s >> rest

hello :: TextIOM ()
hello = do
  putText "Hello! any two key typing!: "
  s <- getText 2
  -- skip newline
  getChar
  putText "Your keys: "
  putText s
  putText "\n"
  s2 <- getText 1
  logText DEBUG "Put!"
  logText INFO s2
  --halt 0

type IOM_ST = StateT Int TextIOM

type IOM_So a = C.Source IOM_ST [a]
type IOM_Co a = C.Conduit [a] IOM_ST [a]
type IOM_Si a = C.Sink [a] IOM_ST ()

sourceIOM :: IOM_So Int
sourceIOM = do
  lift $ lift $ putText "put!"
  C.yield [1]

sinkIOM :: IOM_Si Int
sinkIOM = lift $ put 1

coIOM :: IOM_Co Int
coIOM = do
  lift $ lift $ putText "put!"
  lift $ put 1
  C.yield [1]

stateIOM :: IOM_ST ()
stateIOM = do
  lift $ putText "put!"
  lift $ logText INFO "info"
  x <- lift $ getText 2
  if x == "aa" then do
    lift $ getText 2
    put 1
  else put 2
