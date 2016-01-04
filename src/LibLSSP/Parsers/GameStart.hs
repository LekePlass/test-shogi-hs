module LibLSSP.Parsers.GameStart
  ( readyTime
  ) where

import qualified Data.Attoparsec.Text as AParsec

import qualified LibLSSP.Parsers.Base as PB

readyTime :: AParsec.Parser Int
readyTime = PB.seconds AParsec.<?> "ready time"
