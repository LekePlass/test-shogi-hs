module Data.Shogi.StdTypes
  ( StdShogiPlayer (..)
  , StdShogiComp
  , stdShogiComp
  , StdShogiMoveAction (..)
  , move
  , stdMoveKoma
  , canNariKoma
  ) where

import           Control.Lens
import           Data.Maybe                (fromMaybe)

import           Data.Shogi.Board
import           Data.Shogi.Internal.Board
import           Data.Shogi.Internal.Koma
import           Data.Shogi.Koma

data StdShogiPlayer
  = SentePlayer
  | GotePlayer
  deriving ( Eq, Ord, Show )

type StdShogiComp = ShogiComp StdShogiPlayer

stdShogiIni :: [((Int, Int), Maybe (ShogiBoardKoma StdShogiPlayer))]
stdShogiIni =
  [ ((1, 1), komaSente KomaKyosha  )
  , ((2, 1), komaSente KomaKeima   )
  , ((3, 1), komaSente KomaGinsho  )
  , ((4, 1), komaSente KomaKinsho  )
  , ((5, 1), komaSente KomaOsho    )
  , ((6, 1), komaSente KomaKinsho  )
  , ((7, 1), komaSente KomaGinsho  )
  , ((8, 1), komaSente KomaKeima   )
  , ((9, 1), komaSente KomaKyosha  )
  , ((2, 2), komaSente KomaKakugyo )
  , ((8, 2), komaSente KomaHisha   )
  , ((1, 3), komaSente KomaFuhyo   )
  , ((2, 3), komaSente KomaFuhyo   )
  , ((3, 3), komaSente KomaFuhyo   )
  , ((4, 3), komaSente KomaFuhyo   )
  , ((5, 3), komaSente KomaFuhyo   )
  , ((6, 3), komaSente KomaFuhyo   )
  , ((7, 3), komaSente KomaFuhyo   )
  , ((8, 3), komaSente KomaFuhyo   )
  , ((9, 3), komaSente KomaFuhyo   )
  , ((1, 7), komaGote  KomaFuhyo   )
  , ((2, 7), komaGote  KomaFuhyo   )
  , ((3, 7), komaGote  KomaFuhyo   )
  , ((4, 7), komaGote  KomaFuhyo   )
  , ((5, 7), komaGote  KomaFuhyo   )
  , ((6, 7), komaGote  KomaFuhyo   )
  , ((7, 7), komaGote  KomaFuhyo   )
  , ((8, 7), komaGote  KomaFuhyo   )
  , ((9, 7), komaGote  KomaFuhyo   )
  , ((8, 8), komaGote  KomaKakugyo )
  , ((2, 8), komaGote  KomaHisha   )
  , ((1, 9), komaGote  KomaKyosha  )
  , ((2, 9), komaGote  KomaKeima   )
  , ((3, 9), komaGote  KomaGinsho  )
  , ((4, 9), komaGote  KomaKinsho  )
  , ((5, 9), komaGote  KomaOsho    )
  , ((6, 9), komaGote  KomaKinsho  )
  , ((7, 9), komaGote  KomaGinsho  )
  , ((8, 9), komaGote  KomaKeima   )
  , ((9, 9), komaGote  KomaKyosha  )
  ]
  where
    komaSente = Just . koma SentePlayer
    komaGote  = Just . koma GotePlayer

isStdRange :: (Int, Int) -> Bool
isStdRange (r, c) = 1 <= r && r <= 9 && 1 <= c && c <= 9

canMove :: StdShogiPlayer -> (Int, Int) -> StdShogiComp -> Bool
canMove pid idx sc = isStdRange idx && isOwn
  where
    isOwn = maybe True not $ isOwnKoma pid idx sc

filterCanMove :: StdShogiPlayer -> StdShogiComp -> [(Int, Int)] -> [(Int, Int)]
filterCanMove pid sc = filter $ canMove pid `flip` sc

moveKomaByRange :: StdShogiPlayer -> (Int, Int) -> StdShogiComp -> [(Int, Int)] -> [(Int, Int)]
moveKomaByRange pid (r, c) sc rs = filterCanMove pid sc idxs
  where
    idxs = map biasIx rs

    reverseIdxByPlayer :: StdShogiPlayer -> (Int, Int) -> (Int, Int)
    reverseIdxByPlayer SentePlayer idx      = idx
    reverseIdxByPlayer GotePlayer  (ir, ic) = (-ir, -ic)

    biasIx :: (Int, Int) -> (Int, Int)
    biasIx idx = reverseIdxByPlayer pid idx & _1 %~ (+ r) & _2 %~ (+ c)

moveKomaByDir :: StdShogiPlayer -> (Int, Int) -> StdShogiComp -> (Int, Int) -> [(Int, Int)]
moveKomaByDir pid idx sc dir = idxsT ^. _1 ++ filterCanMove pid sc [head $ idxsT ^. _2]
  where
    b = onboard sc

    idxsT = span canMove' $ tail $ iterate (addTuple (reverseIdxByPlayer pid dir)) idx
    addTuple idx1 (r, c) = idx1 & _1 %~ (+ r) & _2 %~ (+ c)

    reverseIdxByPlayer :: StdShogiPlayer -> (Int, Int) -> (Int, Int)
    reverseIdxByPlayer SentePlayer sidx     = sidx
    reverseIdxByPlayer GotePlayer  (ir, ic) = (-ir, -ic)

    canMove' :: (Int, Int) -> Bool
    canMove' sidx = isStdRange sidx && isNothing (b ! sidx)

stdMoveKoma' :: StdShogiPlayer -> ShogiKoma -> (Int, Int) -> StdShogiComp -> [(Int, Int)]
stdMoveKoma' pid KomaFuhyo   idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(0, 1)]

stdMoveKoma' pid KomaKyosha  idx sc = idxs
  where
    idxs = concatMap canDirMove [(1, 0)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma' pid KomaKeima   idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(-1, 2), (1, 2)]

stdMoveKoma' pid KomaGinsho  idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(-1, -1), (1, -1), (-1, 1), (0, 1), (1, 1)]

stdMoveKoma' pid KomaKinsho  idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

stdMoveKoma' pid KomaOsho    idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [ (-1, -1), (0, -1), (1, -1)
         , (-1,  0)         , (1,  0)
         , (-1,  1), (0,  1), (1,  1)
         ]

stdMoveKoma' pid KomaHisha   idx sc = idxs
  where
    idxs = concatMap canDirMove [(-1, 0), (0, -1), (1, 0), (0, 1)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma' pid KomaKakugyo idx sc = idxs
  where
    idxs = concatMap canDirMove [(-1, -1), (1, -1), (1, 1), (-1, 1)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma' pid KomaTokin   idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

stdMoveKoma' pid KomaNarikyo idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

stdMoveKoma' pid KomaNarikei idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

stdMoveKoma' pid KomaNarigin idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

stdMoveKoma' pid KomaRyuo    idx sc = idxsR ++ idxsD
  where
    idxsR = moveKomaByRange pid idx sc rs
    rs = [(-1, -1), (-1, 1), (1, 1), (1, -1)]

    idxsD = concatMap canDirMove [(-1, 0), (0, -1), (1, 0), (0, 1)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma' pid KomaRyuma   idx sc = idxsR ++ idxsD
  where
    idxsR = moveKomaByRange pid idx sc rs
    rs = [(-1, 0), (0, -1), (1, 0), (0, 1)]

    idxsD = concaMap canDirMove [(-1, -1), (1, -1), (1, 1), (-1, 1)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma :: (Int, Int) -> StdShogiComp -> [(Int, Int)]
stdMoveKoma idx sc = fromMaybe [] $ do
  bsk <- onboard sc ! idx
  return $ stdMoveKoma' (player bsk) (komaId bsk) idx sc

stdShogiComp :: ShogiComp StdShogiPlayer
stdShogiComp = ShogiComp
  { onboard = ob
  , onhands = ohs
  }
  where
    ob' = shogiBoard 9 9
    ob = ob' // stdShogiIni

    ohs = shogiOnHands

data StdShogiMoveAction
  = ShogiMoveOnBoard (Int, Int) (Int, Int) ShogiKoma
  | ShogiMoveOnHand (Int, Int) ShogiKoma
  deriving ( Eq, Show )

move :: StdShogiPlayer -> StdShogiMoveAction -> StdShogiComp -> Maybe StdShogiComp
move pid (ShogiMoveOnBoard idx1 idx2 sk) sc = resc
  where
    resc = if canMoveOnBoard pid idx1 idx2 sk sc && idx2 `elem` skMoves
      then Just $ ShogiComp
        { onboard = b //
          [ (idx1, Nothing)
          , (idx2, Just $ koma pid sk)
          ]
        , onhands = fromMaybe ohs $ do
          psk <- b ! idx2
          return $ insertOnHands pid (komaId psk) ohs
        }
      else Nothing

    b = onboard sc
    ohs = onhands sc

    skMoves = stdMoveKoma idx1 sc

move pid (ShogiMoveOnHand idx sk) sc = resc
  where
    resc = if canMoveOnHands pid idx sk sc
      then do
        pohs <- pickupOnHands pid sk ohs
        return ShogiComp
          { onboard = b // [(idx, Just $ koma pid sk)]
          , onhands = pohs
          }
      else Nothing

    b = onboard sc
    ohs = onhands sc

canMoveOnBoard :: StdShogiPlayer -> (Int, Int) -> (Int, Int) -> ShogiKoma -> StdShogiComp -> Bool
canMoveOnBoard pid idx1 idx2 sk sc = (fromMaybe False $ do
  bsk <- b ! idx1
  let chKoma = canChKoma idx2 (komaId bsk) sk
  let ownKoma1 = pid == player bsk
  return $ chKoma && ownKoma1) &&
  (maybe True not $ do
  ownKoma2 <- isOwnKoma pid idx2 sc
  return ownKoma2)
  where
    b = onboard sc

    canChKoma :: (Int, Int) -> ShogiKoma -> ShogiKoma -> Bool
    canChKoma idx ksk nsk = ksk == nsk ||
      canChRange pid idx && (fromMaybe False $ do
      bsk <- base nsk
      return $ ksk == bsk)

    canChRange :: StdShogiPlayer -> (Int, Int) -> Bool
    canChRange SentePlayer (_, x) = x >= 7
    canChRange GotePlayer  (_, x) = x <= 3

canMoveOnHands :: StdShogiPlayer -> (Int, Int) -> ShogiKoma -> StdShogiComp -> Bool
canMoveOnHands pid idx sk sc = ohc > 0 && isNothing (b ! idx)
  where
    b = onboard sc
    ohs = onhands sc

    ohc = lookupOnHands pid sk ohs

isOwnKoma :: StdShogiPlayer -> (Int, Int) -> StdShogiComp -> Maybe Bool
isOwnKoma pid idx sc = do
  psk <- b ! idx
  return $ pid == player psk
  where
    b = onboard sc

canNariKoma :: StdShogiPlayer -> (Int, Int) -> (Int, Int) -> StdShogiComp -> Maybe ShogiKoma
canNariKoma pid idx1 idx2 sc = do
  sk <- onboard sc ! idx1
  nsk <- nari $ komaId sk
  _ <- move pid (ShogiMoveOnBoard idx1 idx2 nsk) sc
  return nsk
