module LibShogi.Data.ShogiBoard
  ( ShogiBoardKoma
  , ShogiBoard
  , shogiBoard
  , lookupOnBoard
  , ShogiOnHands
  , shogiOnHands
  , lookupOnHands
  , insertOnHands
  , pickupOnHands
  , assocsOnHands
  , ShogiComp (..)
  ) where

import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)

import           LibShogi.Data.Board
import           LibShogi.Data.Koma
import           LibShogi.Data.ShogiKoma

type ShogiBoardKoma a = Koma a ShogiKoma
type ShogiBoard a = Board (Maybe (ShogiBoardKoma a))

shogiBoard :: Ord a => Int -> Int -> ShogiBoard a
shogiBoard w h = board w h Nothing

lookupOnBoard :: Ord a => Int -> Int -> ShogiBoard a -> Maybe (ShogiBoardKoma a)
lookupOnBoard r c b = b ! (r, c)

type ShogiOnHand = Map.Map ShogiKoma Int
type ShogiOnHands a = Map.Map a ShogiOnHand

shogiOnHands :: Ord a => ShogiOnHands a
shogiOnHands = Map.fromList []

lookupOnHands :: Ord a => a -> ShogiKoma -> ShogiOnHands a -> Int
lookupOnHands pid sk ohs = fromMaybe 0 $ do
  oh <- Map.lookup pid ohs
  return $ lookupOnHand sk oh

insertOnHands :: Ord a => a -> ShogiKoma -> ShogiOnHands a -> ShogiOnHands a
insertOnHands pid sk ohs = Map.insert pid noh ohs
  where
    noh = fromMaybe (Map.fromList [(sk, 1)]) $ do
      oh <- Map.lookup pid ohs
      return $ insertOnHand sk oh

pickupOnHands :: Ord a => a -> ShogiKoma -> ShogiOnHands a -> Maybe (ShogiOnHands a)
pickupOnHands pid sk ohs = do
  oh <- Map.lookup pid ohs
  poh <- pickupOnHand sk oh
  return $ Map.insert pid poh ohs

assocsOnHands :: Ord a => a -> ShogiOnHands a -> [(ShogiKoma, Int)]
assocsOnHands pid ohs = fromMaybe [] $ do
  oh <- Map.lookup pid ohs
  return $ Map.assocs oh

lookupOnHand :: ShogiKoma -> ShogiOnHand -> Int
lookupOnHand sk oh = fromMaybe 0 $ Map.lookup sk oh

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
