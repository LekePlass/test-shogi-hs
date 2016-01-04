module LibLSSP.Comps.Base
  ( Version (..)
  , convertVersion3
  , DataFormatInfo (..)
  ) where

import qualified Data.Text as T

data Version
  = Version3 Int Int Int
  | Version3a Int Int Int Char
  deriving ( Eq, Show, Ord )

convertVersion3 :: Version -> (Int, Int, Int)
convertVersion3 (Version3  x y z)   = (x, y, z)
convertVersion3 (Version3a x y z _) = (x, y, z)

data DataFormatInfo = DataFormatInfo
  { name :: T.Text
  , version :: Version
  }
  deriving ( Eq, Show, Ord )