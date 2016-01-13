module Data.Shogi.Internal.Koma
  ( Koma (..)
  , koma
  ) where

data Koma a k = Koma
  { player :: a
  , komaId :: k
  }
  deriving ( Eq )

instance Show k => Show (Koma a k) where
  show (Koma { komaId = kid }) = show kid

koma :: (Ord a, Eq k) => a -> k -> Koma a k
koma pid kv = Koma
  { player = pid
  , komaId = kv
  }
