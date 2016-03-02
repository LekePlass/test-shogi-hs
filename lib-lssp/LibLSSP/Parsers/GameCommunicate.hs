{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Parsers.GameCommunicate
  ( GameContext (..)
  , gameTime
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text          as AParsec
import qualified Data.Text                     as T
import           LibLSSP.Comps.Base            (GameKoma (..))
import qualified LibLSSP.Comps.Base            as Base
import           LibLSSP.Comps.GameCommunicate
import qualified LibLSSP.Parsers.Base          as PB

gameTime :: AParsec.Parser Base.Time
gameTime = PB.seconds AParsec.<?> "game time"

gameActionMode :: AParsec.Parser GameAction
gameActionMode = moveAction <|> resignAction <|> mateAction <|> extraAction
  AParsec.<?> "game action mode"
  where
    moveAction :: AParsec.Parser GameAction
    moveAction = AParsec.string "move"
      *> return MoveAction AParsec.<?> "move"

    resignAction :: AParsec.Parser GameAction
    resignAction = AParsec.string "resign"
      *> return ResignAction AParsec.<?> "resign"

    mateAction :: AParsec.Parser GameAction
    mateAction = AParsec.string "mate"
      *> return MateAction AParsec.<?> "mate"

    extraAction :: AParsec.Parser GameAction
    extraAction = AParsec.string "extra"
      *> return ExtraAction AParsec.<?> "extra"

gameActionMove :: AParsec.Parser GameActionMoveInfo
gameActionMove = do
  let pp = PB.lexeme komaPoint
  let sp = PB.lexeme $ AParsec.char ';'
  idx1 <- pp <* sp
  idx2 <- pp <* sp
  k    <- gameKoma
  let p = case idx1 of
            (0, 0) -> return $ MoveActionOnHand idx2 k
            _      -> return $ MoveActionOnBoard idx1 idx2 k
  p AParsec.<?> "game action move"

komaPoint :: AParsec.Parser (Int, Int)
komaPoint = beginBr *> mtuple <* endBr
  where
    beginBr = PB.lexeme $ AParsec.char '('
    endBr   = AParsec.char ')'

    mtuple = do
      let p = PB.lexeme AParsec.decimal
      x <- p <* AParsec.char ','
      let f y = (x, y)
      f <$> p AParsec.<?> "koma point"

gameKoma :: AParsec.Parser GameKoma
gameKoma = do
  xs <- AParsec.take 2
  chKomaStr xs AParsec.<?> "game koma"
  where
    chKomaStr :: T.Text -> AParsec.Parser GameKoma
    chKomaStr s
      | s == "FU" = return KomaFuhyo
      | s == "KY" = return KomaKyosha
      | s == "KE" = return KomaKeima
      | s == "GI" = return KomaGinsho
      | s == "KI" = return KomaKinsho
      | s == "KA" = return KomaKakugyo
      | s == "HI" = return KomaHisha
      | s == "OU" = return KomaOsho
      | s == "TO" = return KomaTokin
      | s == "NY" = return KomaNarikyo
      | s == "NK" = return KomaNarikei
      | s == "NG" = return KomaNarigin
      | s == "UM" = return KomaRyuma
      | s == "RY" = return KomaRyuo
      | otherwise        = empty

gameStopTime :: AParsec.Parser Base.Time
gameStopTime = PB.seconds AParsec.<?> "game stop time"

gameStatus :: AParsec.Parser GameStatus
gameStatus = continue <|> end AParsec.<?> "game status"
  where
    continue :: AParsec.Parser GameStatus
    continue = AParsec.string "continue"
      *> return GameContinue AParsec.<?> "continue"

    end :: AParsec.Parser GameStatus
    end = AParsec.string "end"
      *> return GameEnd AParsec.<?> "end"
