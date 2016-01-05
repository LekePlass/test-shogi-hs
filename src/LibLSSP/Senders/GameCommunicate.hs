module LibLSSP.Senders.GameCommunicate
  ( gameTimeCommand
  , goCommand
  , gameActionModeCommand
  , gameStopCommand
  , gameStopTimeCommand
  , gameStopReceivedCommand
  , gameStatusCommand
  ) where

import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Comps.GameCommunicate
import           LibLSSP.Senders.Base

gameTimeCommand :: Base.Time -> T.Text
gameTimeCommand gt = withend
  $          T.pack "Game-Time?: "
  `T.append` showTTime gt

goCommand :: T.Text -> T.Text
goCommand s = withend
  $ T.pack "Go: " `T.append` s

gameActionModeCommand :: GameAction -> T.Text
gameActionModeCommand MoveAction   = withend
  $ T.pack "Game-Action-Mode: move"
gameActionModeCommand ResignAction = withend
  $ T.pack "Game-Action-Mode: resign"
gameActionModeCommand MateAction   = withend
  $ T.pack "Game-Action-Mode: mate"
gameActionModeCommand ExtraAction  = withend
  $ T.pack "Game-Action-Mode: extra"

gameStopCommand :: T.Text -> T.Text
gameStopCommand s = withend
  $ T.pack "Game-Stop: " `T.append` s

gameStopTimeCommand :: Base.Time -> T.Text
gameStopTimeCommand gst = withend
  $          T.pack "Game-Stop-Time?: "
  `T.append` showTTime gst

gameStopReceivedCommand :: T.Text -> T.Text
gameStopReceivedCommand s = withend
  $ T.pack "Game-Stop-Received: " `T.append` s

gameStatusCommand :: GameStatus -> T.Text
gameStatusCommand GameContinue = withend
  $ T.pack "Game-Status: continue"
gameStatusCommand GameEnd      = withend
  $ T.pack "Game-Status: end"
