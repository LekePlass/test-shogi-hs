module LibLSSP.Senders.GameEnd
  ( gameEndCommand
  ) where

import qualified Data.Text as T

import           LibLSSP.Comps.GameEnd
import           LibLSSP.Senders.Base

gameEndCommand :: GameEndInfo -> T.Text
gameEndCommand GameEndWin    = withend
  $ T.pack "Game-End: win"
gameEndCommand GameEndLose   = withend
  $ T.pack "Game-End: lose"
gameEndCommand GameEndDraw   = withend
  $ T.pack "Game-End: draw"
gameEndCommand GameEndNogame = withend
  $ T.pack "Game-End: nogame"
