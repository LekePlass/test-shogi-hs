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
  | KomaOsho
  | KomaHisha
  | KomaKakugyo
  | KomaTokin
  | KomaNarikyo
  | KomaNarikei
  | KomaNarigin
  | KomaRyuo
  | KomaRyuma
  deriving ( Eq, Ord )

instance Show ShogiKoma where
  show KomaFuhyo   = "歩"
  show KomaKyosha  = "香"
  show KomaKeima   = "桂"
  show KomaGinsho  = "銀"
  show KomaKinsho  = "金"
  show KomaOsho    = "王"
  show KomaHisha   = "飛"
  show KomaKakugyo = "角"
  show KomaTokin   = "と"
  show KomaNarikyo = "杏"
  show KomaNarikei = "圭"
  show KomaNarigin = "全"
  show KomaRyuo    = "龍"
  show KomaRyuma   = "馬"

nari :: ShogiKoma -> Maybe ShogiKoma
nari KomaFuhyo   = Just KomaTokin
nari KomaKyosha  = Just KomaNarikyo
nari KomaKeima   = Just KomaNarikei
nari KomaGinsho  = Just KomaNarigin
nari KomaHisha   = Just KomaRyuo
nari KomaKakugyo = Just KomaRyuma
nari _           = Nothing

base :: ShogiKoma -> Maybe ShogiKoma
base KomaTokin   = Just KomaFuhyo
base KomaNarikyo = Just KomaKyosha
base KomaNarikei = Just KomaKeima
base KomaNarigin = Just KomaGinsho
base KomaRyuo    = Just KomaHisha
base KomaRyuma   = Just KomaKakugyo
base _           = Nothing

isNarikoma :: ShogiKoma -> Bool
isNarikoma KomaTokin   = True
isNarikoma KomaNarikyo = True
isNarikoma KomaNarikei = True
isNarikoma KomaNarigin = True
isNarikoma KomaRyuo    = True
isNarikoma KomaRyuma   = True
isNarikoma _           = False
