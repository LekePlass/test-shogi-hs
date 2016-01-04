module LibLSSP.Comps.Connect
  ( ProtocolInfo (..)
  ) where

import qualified Data.Text as T

import LibLSSP.Comps.Base

data ProtocolInfo = ProtocolInfo
  { name :: T.Text
  , version :: Version
  }
  deriving ( Eq, Show, Ord )
