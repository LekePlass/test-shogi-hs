module LibLSSP.Comps.Base
  ( Version (..)
  , convertVersion3
  , DataFormatInfo (..)
  , Time (..)
  , DetailInfo (..)
  , GameKoma (..)
  ) where

import qualified Data.Text as T

data Version
  = Version3 Int Int Int
  | Version3a Int Int Int Char
  deriving ( Eq, Ord )

instance Show Version where
  show (Version3 x y z)
    = show x ++ "." ++ show y ++ "." ++ show z
  show (Version3a x y z c)
    = show x ++ "." ++ show y ++ "." ++ show z ++ [c]

convertVersion3 :: Version -> (Int, Int, Int)
convertVersion3 (Version3  x y z)   = (x, y, z)
convertVersion3 (Version3a x y z _) = (x, y, z)

data DataFormatInfo = DataFormatInfo
  { name    :: T.Text
  , version :: Version
  }
  deriving ( Eq, Show, Ord )

data Time = Seconds Int

data DetailInfo = DetailInfo
  { detailType    :: T.Text
  , message       :: Maybe T.Text
  , detailMessage :: Maybe T.Text
  }

data GameKoma
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

instance Show GameKoma where
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
