module LibLSSP.Comps.RuleConsensus
  ( SetOptionsInfo (..)
  , RuleDeclarationInfo (..)
  , RuleCustomizeInfo (..)
  , InitialContext (..)
  ) where

import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base

data SetOptionsInfo = SetOptionsInfo
  { rules :: RuleCustomizeInfo
  }

data RuleDeclarationInfo = RuleDeclarationInfo
  { name :: T.Text
  , version :: Base.Version
  }

data RuleCustomizeInfo = RuleCustomizeInfo
  { isWaiting :: Bool
  }

data InitialContext = InitialContext
  { maxMoves :: Maybe Int
  }
