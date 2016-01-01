module LibShogi.Data.Shogi
  ( ShogiKoma(..)
  , nari
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

nari :: ShogiKoma -> Maybe ShogiKoma
nari KomaFuhyo   = Just KomaTokin
nari KomaKyosha  = Just KomaNarikyo
nari KomaKeima   = Just KomaNarikei
nari KomaGinsho  = Just KomaNarigin
nari KomaHisha   = Just KomaRyuou
nari KomaKakugyo = Just KomaRyuma
nari _           = Nothing

isNarikoma :: ShogiKoma -> Bool
isNarikoma KomaTokin   = True
isNarikoma KomaNarikyo = True
isNarikoma KomaNarikei = True
isNarikoma KomaNarigin = True
isNarikoma KomaRyuou   = True
isNarikoma KomaRyuma   = True
isNarikoma _           = False
