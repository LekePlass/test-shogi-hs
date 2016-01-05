module LibLSSP.Parsers.GameCommunicate
  ( GameContext (..)
  , gameTime
  ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Attoparsec.Text as AParsec
import qualified Data.Text as T
import           Data.Maybe (catMaybes)

import qualified LibLSSP.Parsers.Base as PB
import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Comps.GameCommunicate

instance FromJSON GameContext where
  parseJSON (Object v)
    = GameContext
    <$> PB.paramOr (v .:) (T.pack "max_moves")
  parseJSON _           = empty

instance ToJSON GameContext where
  toJSON v = object $ catMaybes
    [ T.pack "max_moves" `maybeElem` maxMoves v
    ]
    where
      maybeElem k ma = (k .=) <$> ma

gameTime :: AParsec.Parser Base.Time
gameTime = PB.seconds AParsec.<?> "game time"

gameActionMode :: AParsec.Parser GameAction
gameActionMode = moveAction <|> resignAction <|> mateAction <|> extraAction
  AParsec.<?> "game action mode"
  where
    moveAction :: AParsec.Parser GameAction
    moveAction = (AParsec.string $ T.pack "move")
      *> return MoveAction AParsec.<?> "move"
    
    resignAction :: AParsec.Parser GameAction
    resignAction = (AParsec.string $ T.pack "resign")
      *> return ResignAction AParsec.<?> "resign"
    
    mateAction :: AParsec.Parser GameAction
    mateAction = (AParsec.string $ T.pack "mate")
      *> return MateAction AParsec.<?> "mate"
    
    extraAction :: AParsec.Parser GameAction
    extraAction = (AParsec.string $ T.pack "extra")
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
      | s == T.pack "FU" = return KomaFuhyo
      | s == T.pack "KY" = return KomaKyosha
      | s == T.pack "KE" = return KomaKeima
      | s == T.pack "GI" = return KomaGinsho
      | s == T.pack "KI" = return KomaKinsho
      | s == T.pack "KA" = return KomaKakugyo
      | s == T.pack "HI" = return KomaHisha
      | s == T.pack "OU" = return KomaOsho
      | s == T.pack "TO" = return KomaTokin
      | s == T.pack "NY" = return KomaNarikyo
      | s == T.pack "NK" = return KomaNarikei
      | s == T.pack "NG" = return KomaNarigin
      | s == T.pack "UM" = return KomaRyuma
      | s == T.pack "RY" = return KomaRyuo
      | otherwise        = empty

gameStopTime :: AParsec.Parser Base.Time
gameStopTime = PB.seconds AParsec.<?> "game stop time"

gameStatus :: AParsec.Parser GameStatus
gameStatus = continue <|> end AParsec.<?> "game status"
  where
    continue :: AParsec.Parser GameStatus
    continue = (AParsec.string $ T.pack "continue")
      *> return GameContinue AParsec.<?> "continue"
    
    end :: AParsec.Parser GameStatus
    end = (AParsec.string $ T.pack "end")
      *> return GameEnd AParsec.<?> "end"
