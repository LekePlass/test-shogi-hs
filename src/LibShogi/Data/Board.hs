module LibShogi.Data.Board 
  ( board
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
      
      showLine' 0 y = showItem 0 y
      showLine' x y = showLine' (x - 1) y ++ ", " ++ showItem x y
      showLine x = "[" ++ showLine' (w - 1) x ++ "]"
      
      showStr' 0 = showLine 0
      showStr' x = showStr' (x - 1) ++ ", " ++ showLine x
      showStr = "[" ++ showStr' (h - 1) ++ "]"

board :: Int -> Int -> a -> Board a
board w h i = Board
  { width  = w
  , height = h
  , items  = Arr.listArray ((0, 0), (w - 1, h - 1)) $
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
