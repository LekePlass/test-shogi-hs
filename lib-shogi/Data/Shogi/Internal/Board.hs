module Data.Shogi.Internal.Board
  ( Board (..)
  , board
  , index
  , (!)
  , elems
  , assocs
  , set
  , (//)
  ) where

import qualified Data.Array as Arr

data Board a = Board
  { width  :: Int
  , height :: Int
  , items  :: Arr.Array (Int, Int) a
  }
  deriving ( Eq )

instance Show a => Show (Board a) where
  show (Board { width = w, height = h, items = arr }) = showStr
    where
      showItem x y = show $ arr Arr.! (x, y)

      showLine' 1 y = showItem 1 y
      showLine' x y = showLine' (x - 1) y ++ ", " ++ showItem x y
      showLine x = "[" ++ showLine' w x ++ "]"

      showStr' 1 = showLine 1
      showStr' x = showStr' (x - 1) ++ ", " ++ showLine x
      showStr = "[" ++ showStr' h ++ "]"

board :: Int -> Int -> a -> Board a
board w h i = Board
  { width  = w
  , height = h
  , items  = Arr.listArray ((1, 1), (w, h)) $
    iterate id i
  }

index :: Board a -> (Int, Int) -> a
index b idx = items b Arr.! idx

(!) :: Board a -> (Int, Int) -> a
(!) = index

elems :: Board a -> [a]
elems b = Arr.elems $ items b

assocs :: Board a -> [((Int, Int), a)]
assocs b = Arr.assocs $ items b

set :: Board a -> [((Int, Int), a)] -> Board a
set b idxs = Board
  { width  = width b
  , height = height b
  , items  = items b Arr.// idxs
  }

(//) :: Board a -> [((Int, Int), a)] -> Board a
(//) = set
