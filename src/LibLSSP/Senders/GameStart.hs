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
isReadyCommand s = withend
  $ T.pack "Is-Ready: " `T.append` s

readyTimeCommand :: Base.Time -> T.Text
readyTimeCommand rt = withend
  $          T.pack "Ready-Time?: "
  `T.append` showTTime rt

readyGameCommand :: T.Text -> T.Text
readyGameCommand s = withend
  $ T.pack "Ready-Game: " `T.append` s

gameStartCommand :: T.Text -> T.Text
gameStartCommand s = withend
  $ T.pack "Game-Start: " `T.append` s
