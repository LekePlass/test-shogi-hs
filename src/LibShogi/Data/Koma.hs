module LibShogi.Data.Koma
  ( Koma(..)
  , koma
  ) where

data Koma a k = Koma 
  { player :: a
  , komaId :: k
  }

koma :: (Ord a, Eq k) => a -> k -> Koma a k
koma pid kv = Koma
  { player = pid
  , komaId = kv
  }


