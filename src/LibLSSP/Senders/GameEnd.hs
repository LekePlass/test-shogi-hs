{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Senders.GameEnd
  ( gameEndCommand
  ) where

import qualified Data.Text             as T

import           LibLSSP.Comps.GameEnd
import           LibLSSP.Senders.Base

gameEndCommand :: GameEndInfo -> T.Text
gameEndCommand GameEndWin    = withend "Game-End: win"
gameEndCommand GameEndLose   = withend "Game-End: lose"
gameEndCommand GameEndDraw   = withend "Game-End: draw"
gameEndCommand GameEndNogame = withend "Game-End: nogame"
