module LibLSSP.Comps.GameCommunicate
  ( GameContext (..)
  , GameAction (..)
  , GameActionMoveInfo (..)
  , GameStatus (..)
  , GameStatusResult (..)
  ) where

import qualified Data.Map           as Map
import qualified LibLSSP.Comps.Base as Base

data GameContext = GameContext
  { board  :: [[Maybe Base.GameKoma]]
  , colors :: [[Bool]]
  , hands  :: Map.Map Bool [Base.GameKoma]
  }
  deriving ( Eq, Show, Ord )

data GameAction
  = MoveAction
  | ResignAction
  | MateAction
  | ExtraAction
  deriving ( Eq, Show, Ord )

data GameActionMoveInfo
  = MoveActionOnBoard (Int, Int) (Int, Int) Base.GameKoma
  | MoveActionOnHand             (Int, Int) Base.GameKoma
  deriving ( Eq, Show, Ord )

data GameStatus
  = GameContinue
  | GameEnd
  deriving ( Eq, Show, Ord )

data GameStatusResult = GameStatusResult
  { boardS  :: [[Maybe Base.GameKoma]]
  , colorsS :: [[Bool]]
  , handsS  :: Map.Map Bool [Base.GameKoma]
  }
