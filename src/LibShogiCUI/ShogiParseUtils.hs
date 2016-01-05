{-# LANGUAGE OverloadedStrings #-}

module LibShogiCUI.ShogiParseUtils 
  ( showText
  , ConsoleShogiMoveAction (..)
  , parseMoveAction
  , parserMoveAction
  , koma
  , point
  ) where

import qualified Data.Attoparsec.Text as AParsec
import qualified Data.List as List
import qualified Data.Text as T
import           Control.Applicative

import LibShogi.Data.ShogiKoma

showText :: Show a => a -> T.Text
showText = T.pack . show

data ConsoleShogiMoveAction
  = CSActionOnBoard (Int, Int) (Int, Int) (Maybe ShogiKoma)
  | CSActionOnHand  (Int, Int) ShogiKoma
  deriving ( Eq, Ord, Show )

parseMoveAction :: T.Text -> Maybe ConsoleShogiMoveAction
parseMoveAction s = case AParsec.parseOnly parserMoveAction $ T.toUpper s of
  Right x -> Just x
  _       -> Nothing

parserMoveAction :: AParsec.Parser ConsoleShogiMoveAction
parserMoveAction = csactionOnBoard <|> csactionOnHand
  where  
    lexeme p = p <* AParsec.skipSpace
    lexemeSemicol = lexeme $ AParsec.char ';'
    
    csactionOnBoard = CSActionOnBoard
      <$> (AParsec.skipSpace *> lexeme point <* lexemeSemicol)
      <*> lexeme point
      <*> ((lexemeSemicol *> (Just <$> lexeme koma)) <|> return Nothing)
    
    csactionOnHand = CSActionOnHand
      <$> (AParsec.skipSpace *> lexeme point <* lexemeSemicol)
      <*> lexeme koma

koma :: AParsec.Parser ShogiKoma
koma = List.foldl1 (<|>) convKomaParsers
  where
    convKomaList :: [([T.Text], ShogiKoma)]
    convKomaList =
      [ (["P", "FU", "FUHYO", "歩", "歩兵"], KomaFuhyo)
      , (["+P", "TO", "TOKIN", "と", "と金"], KomaTokin)
      , (["L", "KY", "KYOSHA", "香", "香車"], KomaKyosha)
      , (["+L", "NY", "NARIKYO", "杏", "成香"], KomaNarikyo)
      , (["N", "KE", "KEIMA", "桂", "桂馬"], KomaKeima)
      , (["+N", "NK", "NARIKEI", "圭", "成桂"], KomaNarikei)
      , (["S", "GI", "GINSHO", "銀", "銀将"], KomaGinsho)
      , (["+S", "NG", "NARIGIN", "全", "成銀"], KomaNarigin)
      , (["G", "KI", "KINSHO", "金", "金将"], KomaKinsho)
      , (["R", "HI", "HISHA", "飛", "飛車"], KomaHisha)
      , (["+R", "RY", "RYUO", "龍", "龍王"], KomaRyuo)
      , (["B", "KA", "KAKUGYO", "角", "角行"], KomaKakugyo)
      , (["+B", "UM", "RYUMA", "馬", "龍馬"], KomaRyuma)
      , (["K", "OU", "OSHO", "王", "王将"], KomaOsho)
      ]
    
    convKomaParsers =
      [ List.foldl1 (<|>) [ AParsec.string kStr | kStr <- kStrs] *> return k
      | (kStrs, k) <- convKomaList
      ]

point :: AParsec.Parser (Int, Int)
point = (AParsec.char '(' *> mtuple <* AParsec.char ')') <|> mtuple
  where
    int :: AParsec.Parser Int
    int = AParsec.signed AParsec.decimal
    
    mtuple = do
      let p = AParsec.skipSpace *> int <* AParsec.skipSpace
      x <- p <* AParsec.char ','
      let f y = (x, y)
      f <$> p
