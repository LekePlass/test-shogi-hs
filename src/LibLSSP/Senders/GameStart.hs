{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Senders.GameStart
  ( isReadyCommand
  , readyTimeCommand
  , readyGameCommand
  , gameStartCommand
  ) where

import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Senders.Base

isReadyCommand :: T.Text -> T.Text
isReadyCommand s = withend $ "Is-Ready: " `T.append` s

readyTimeCommand :: Base.Time -> T.Text
readyTimeCommand rt = withend $ "Ready-Time?: " `T.append` showTTime rt

readyGameCommand :: T.Text -> T.Text
readyGameCommand s = withend $ "Ready-Game: " `T.append` s

gameStartCommand :: T.Text -> T.Text
gameStartCommand s = withend $ "Game-Start: " `T.append` s
