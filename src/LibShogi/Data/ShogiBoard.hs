module LibShogi.Data.ShogiBoard 
  ( ShogiPlayer(..)
  ) where

import LibShogi.Data.Koma
import LibShogi.Data.Board ()
import LibShogi.Data.Shogi

data ShogiPlayer
  = SentePlayer
  | GotePlayer

type ShogiBoardKoma = Koma ShogiPlayer ShogiKoma


