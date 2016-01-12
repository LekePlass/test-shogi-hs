module LibLSSP.Comps.GameEnd
  ( GameEndInfo (..)
  ) where

data GameEndInfo
  = GameEndWin
  | GameEndLose
  | GameEndDraw
  | GameEndNogame
  deriving ( Eq, Show, Ord )
