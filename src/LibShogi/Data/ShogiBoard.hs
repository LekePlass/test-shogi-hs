module LibShogi.Data.ShogiBoard 
  ( shogiBoard
  , shogiOnHands
  , lookupOnHands
  , insertOnHands
  , pickupOnHands
  , ShogiComp (..)
  ) where

import qualified Data.Map as Map

import LibShogi.Data.Koma
import LibShogi.Data.Board
import LibShogi.Data.Shogi

type ShogiBoardKoma a = Koma a ShogiKoma
type ShogiBoard a = Board (Maybe (ShogiBoardKoma a))

shogiBoard :: Ord a => Int -> Int -> ShogiBoard a
shogiBoard w h = board w h Nothing

type ShogiOnHand = Map.Map ShogiKoma Int
type ShogiOnHands a = Map.Map a ShogiOnHand

shogiOnHands :: Ord a => ShogiOnHands a
shogiOnHands = Map.fromList []

lookupOnHands :: Ord a => a -> ShogiOnHands a -> ShogiOnHand
lookupOnHands pid ohs = case Map.lookup pid ohs of
  Just x -> x
  Nothing -> Map.fromList []

insertOnHands :: Ord a => a -> ShogiKoma -> ShogiOnHands a -> ShogiOnHands a
insertOnHands pid sk ohs = Map.insert pid (insertOnHand sk $ lookupOnHands pid ohs) ohs

pickupOnHands :: Ord a => a -> ShogiKoma -> ShogiOnHands a -> Maybe (ShogiOnHands a)
pickupOnHands pid sk ohs = do
  oh <- pickupOnHand sk $ lookupOnHands pid ohs
  return $ Map.insert pid oh ohs

lookupOnHand :: ShogiKoma -> ShogiOnHand -> Int
lookupOnHand sk oh = maybe 0 id $ Map.lookup sk oh

insertOnHand :: ShogiKoma -> ShogiOnHand -> ShogiOnHand
insertOnHand sk oh = Map.insert sk (lookupOnHand sk oh + 1) oh

pickupOnHand :: ShogiKoma -> ShogiOnHand -> Maybe ShogiOnHand
pickupOnHand sk oh = case lookupOnHand sk oh of
  0 -> Nothing
  x -> Just $ Map.insert sk (x - 1) oh

data ShogiComp a = ShogiComp
  { onboard :: ShogiBoard a
  , onhands :: ShogiOnHands a
  }
  deriving ( Eq, Show )

