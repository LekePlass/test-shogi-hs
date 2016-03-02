{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TextIO.Network
  ( runTIOTCPServer
  , serverSettings
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Operational
import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.UTF8      as B8
import           Data.Conduit
import           Data.Conduit.Network
import qualified Data.Text.Encoding        as E
import           Data.TextIO.Internal

type TCPConduitM = ConduitM B.ByteString B.ByteString IO

runTIOTCPServer :: ServerSettings -> TextIO a -> IO a
runTIOTCPServer settings tio = runTCPServer settings app
  where
    app :: AppData -> IO ()
    app appData
      = let pexe = void $ pexec tio ""
      in appSource appData $$ pexe $= appSink appData

    pexec :: TextIO a -> B.ByteString -> TCPConduitM a
    pexec tio' = eval $ view tio'

    eval :: ProgramView TextIOOpe a -> B.ByteString -> TCPConduitM a
    eval (PutText x :>>= is) bs = do
      yield $ E.encodeUtf8 x
      pexec (is ()) bs
    eval (GetCh     :>>= is) bs = evalGetCh is bs
    eval (Throw   x :>>= is) bs = do
      yield $ E.encodeUtf8 x
      yield "\n"
      throwM x
    eval (Return x)          _  = return x

    evalGetCh :: (Char -> TextIO a) -> B.ByteString -> TCPConduitM a
    evalGetCh f bs = case B8.decode bs of
      Just (x,i) -> pexec (f x) $ B.drop i bs
      Nothing -> do
        x <- await
        evalGetCh f $ maybe bs (bs `B.append`) x
