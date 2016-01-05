{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Parsers.GameEnd
  ( gameEnd
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as AParsec
import qualified Data.Text as T ()

import LibLSSP.Comps.GameEnd

gameEnd :: AParsec.Parser GameEndInfo
gameEnd = win <|> lose <|> draw <|> nogame AParsec.<?> "game end"
  where
    win :: AParsec.Parser GameEndInfo
    win = AParsec.string "win"
      *> return GameEndWin AParsec.<?> "win"
    
    lose :: AParsec.Parser GameEndInfo
    lose = AParsec.string "lose"
      *> return GameEndLose AParsec.<?> "lose"
    
    draw :: AParsec.Parser GameEndInfo
    draw = AParsec.string "draw"
      *> return GameEndDraw AParsec.<?> "draw"
    
    nogame :: AParsec.Parser GameEndInfo
    nogame = AParsec.string "nogame"
      *> return GameEndNogame AParsec.<?> "nogame"
