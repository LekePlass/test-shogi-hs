module LibLSSP.Parsers.GameStart
  ( readyTime
  ) where

import qualified Data.Attoparsec.Text as AParsec

import qualified LibLSSP.Comps.Base   as Base
import qualified LibLSSP.Parsers.Base as PB

readyTime :: AParsec.Parser Base.Time
readyTime = PB.seconds AParsec.<?> "ready time"
