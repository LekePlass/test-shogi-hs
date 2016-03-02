{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TextIO.WebSocket
  ( runTIOWSServer
  , runTIOWSServer2
  ) where

import           Control.Monad.Catch
import           Control.Monad.Operational
import qualified Data.Text                 as T
import           Data.TextIO.Internal
import qualified Network.WebSockets        as WS
import qualified Data.Text.IO as T

runTIOWSServer :: String -> Int -> TextIO a -> IO ()
runTIOWSServer host port tio = WS.runServer host port app
  where
    app :: WS.ServerApp
    app pending = do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn 30
      pexec conn tio ""
      return ()

    pexec :: WS.Connection -> TextIO a -> T.Text -> IO a
    pexec conn tio' = eval conn $ view tio'

    eval :: WS.Connection -> ProgramView TextIOOpe a -> T.Text -> IO a
    eval conn (PutText x :>>= is) tx = do
      T.putStrLn tx
      WS.sendTextData conn tx
      pexec conn (is ()) tx
    eval conn (GetCh     :>>= is) tx = evalGetCh conn is tx
    eval conn (Throw   x :>>= is) tx = do
      WS.sendTextData conn $ x `T.append` "\n"
      throwM x
    eval conn (Return x)          tx = return x

    evalGetCh :: WS.Connection -> (Char -> TextIO a) -> T.Text -> IO a
    evalGetCh conn f tx = if T.length tx > 0
      then pexec conn (f $ T.head tx) $ T.tail tx
      else do
        tx' <- WS.receiveData conn
        evalGetCh conn f $ tx `T.append` tx' `T.append` "\n"

runTIOWSServer2 :: String -> Int -> TextIO a -> IO ()
runTIOWSServer2 host port tio = WS.runServer host port app
  where
    app :: WS.ServerApp
    app pending = do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn 30
      tx' <- WS.receiveData conn
      WS.sendTextData conn ("www" `T.append` tx')
      tx <- WS.receiveData conn
      WS.sendTextData conn (tx :: T.Text)
      return ()