module LibLSSP.Comps.GameCommunicate
  ( GameContext (..)
  , GameAction (..)
  , GameKoma (..)
  , GameActionMoveInfo (..)
  , GameStatus (..)
  , GameStatusResult (..)
  ) where

data GameContext = GameContext
  { maxMoves :: Maybe Int
  }
  deriving ( Eq, Show, Ord )

data GameAction
  = MoveAction
  | ResignAction
  | MateAction
  | ExtraAction
  deriving ( Eq, Show, Ord )

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

data GameActionMoveInfo
  = MoveActionOnBoard (Int, Int) (Int, Int) GameKoma
  | MoveActionOnHand             (Int, Int) GameKoma
  deriving ( Eq, Show, Ord )

data GameStatus
  = GameContinue
  | GameEnd
  deriving ( Eq, Show, Ord )

data GameStatusResult = GameStatusResult
  { useTime :: Maybe Int
  }
