{-# LANGUAGE OverloadedStrings #-}

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
gameTimeCommand gt = withend $ "Game-Time?: " `T.append` showTTime gt

goCommand :: T.Text -> T.Text
goCommand s = withend $ "Go: " `T.append` s

gameActionModeCommand :: GameAction -> T.Text
gameActionModeCommand MoveAction   = withend "Game-Action-Mode: move"
gameActionModeCommand ResignAction = withend "Game-Action-Mode: resign"
gameActionModeCommand MateAction   = withend "Game-Action-Mode: mate"
gameActionModeCommand ExtraAction  = withend "Game-Action-Mode: extra"

gameStopCommand :: T.Text -> T.Text
gameStopCommand s = withend $ "Game-Stop: " `T.append` s

gameStopTimeCommand :: Base.Time -> T.Text
gameStopTimeCommand gst = withend $ "Game-Stop-Time?: " `T.append` showTTime gst

gameStopReceivedCommand :: T.Text -> T.Text
gameStopReceivedCommand s = withend $ "Game-Stop-Received: " `T.append` s

gameStatusCommand :: GameStatus -> T.Text
gameStatusCommand GameContinue = withend "Game-Status: continue"
gameStatusCommand GameEnd      = withend "Game-Status: end"
