module LibLSSP.Parsers.GameEnd
  ( gameEnd
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as AParsec
import qualified Data.Text as T

import LibLSSP.Comps.GameEnd

gameEnd :: AParsec.Parser GameEndInfo
gameEnd = win <|> lose <|> draw <|> nogame AParsec.<?> "game end"
  where
    win :: AParsec.Parser GameEndInfo
    win = (AParsec.string $ T.pack "win")
      *> return GameEndWin AParsec.<?> "win"
    
    lose :: AParsec.Parser GameEndInfo
    lose = (AParsec.string $ T.pack "lose")
      *> return GameEndLose AParsec.<?> "lose"
    
    draw :: AParsec.Parser GameEndInfo
    draw = (AParsec.string $ T.pack "draw")
      *> return GameEndDraw AParsec.<?> "draw"
    
    nogame :: AParsec.Parser GameEndInfo
    nogame = (AParsec.string $ T.pack "nogame")
      *> return GameEndNogame AParsec.<?> "nogame"
