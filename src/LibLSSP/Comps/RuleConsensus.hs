module LibLSSP.Comps.RuleConsensus
  ( SetOptionsInfo (..)
  , RuleMode (..)
  , RuleDeclarationInfo (..)
  , RuleCustomizeInfo (..)
  , InitialContext (..)
  ) where

import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base

data SetOptionsInfo = SetOptionsInfo
  { rules :: RuleCustomizeInfo
  }
  deriving ( Eq, Show, Ord )

data RuleMode
  = Declaration
  | Customize
  deriving ( Eq, Show, Ord )

data RuleDeclarationInfo = RuleDeclarationInfo
  { name :: T.Text
  , version :: Base.Version
  }
  deriving ( Eq, Show, Ord )

data RuleCustomizeInfo = RuleCustomizeInfo
  { isWaiting :: Bool
  }
  deriving ( Eq, Show, Ord )

data InitialContext = InitialContext
  { maxMoves :: Maybe Int
  }
  deriving ( Eq, Show, Ord )
