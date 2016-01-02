module LibShogi.Data.ShogiKoma
  ( ShogiKoma (..)
  , nari
  , base
  , isNarikoma
  ) where

data ShogiKoma
  = KomaFuhyo
  | KomaKyosha
  | KomaKeima
  | KomaGinsho
  | KomaKinsho
  | KomaOusho
  | KomaHisha
  | KomaKakugyo
  | KomaTokin
  | KomaNarikyo
  | KomaNarikei
  | KomaNarigin
  | KomaRyuou
  | KomaRyuma
  deriving ( Eq, Ord )

instance Show ShogiKoma where
  show KomaFuhyo   = "歩"
  show KomaKyosha  = "香"
  show KomaKeima   = "桂"
  show KomaGinsho  = "銀"
  show KomaKinsho  = "金"
  show KomaOusho   = "王"
  show KomaHisha   = "飛"
  show KomaKakugyo = "角"
  show KomaTokin   = "と"
  show KomaNarikyo = "杏"
  show KomaNarikei = "圭"
  show KomaNarigin = "全"
  show KomaRyuou   = "龍"
  show KomaRyuma   = "馬"

nari :: ShogiKoma -> Maybe ShogiKoma
nari KomaFuhyo   = Just KomaTokin
nari KomaKyosha  = Just KomaNarikyo
nari KomaKeima   = Just KomaNarikei
nari KomaGinsho  = Just KomaNarigin
nari KomaHisha   = Just KomaRyuou
nari KomaKakugyo = Just KomaRyuma
nari _           = Nothing

base :: ShogiKoma -> Maybe ShogiKoma
base KomaTokin   = Just KomaFuhyo
base KomaNarikyo = Just KomaKyosha
base KomaNarikei = Just KomaKeima
base KomaNarigin = Just KomaGinsho
base KomaRyuou   = Just KomaHisha
base KomaRyuma   = Just KomaKakugyo
base _           = Nothing

isNarikoma :: ShogiKoma -> Bool
isNarikoma KomaTokin   = True
isNarikoma KomaNarikyo = True
isNarikoma KomaNarikei = True
isNarikoma KomaNarigin = True
isNarikoma KomaRyuou   = True
isNarikoma KomaRyuma   = True
isNarikoma _           = False
