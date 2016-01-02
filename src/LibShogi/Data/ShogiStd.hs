module LibShogi.Data.ShogiStd
  ( StdShogiPlayer (..)
  , StdShogiComp
  , stdShogiComp
  , StdShogiMoveAction (..)
  , move
  , stdMoveKoma
  ) where

import Control.Lens

import LibShogi.Data.Koma
import LibShogi.Data.Board
import LibShogi.Data.ShogiKoma
import LibShogi.Data.ShogiBoard

data StdShogiPlayer
  = SentePlayer
  | GotePlayer
  deriving ( Eq, Ord, Show )

type StdShogiComp = ShogiComp StdShogiPlayer

stdShogiIni :: [((Int, Int), Maybe (ShogiBoardKoma StdShogiPlayer))]
stdShogiIni = 
  [ ((0, 0), komaSente KomaKyosha  )
  , ((1, 0), komaSente KomaKeima   )
  , ((2, 0), komaSente KomaGinsho  )
  , ((3, 0), komaSente KomaKinsho  )
  , ((4, 0), komaSente KomaOusho   )
  , ((5, 0), komaSente KomaKinsho  )
  , ((6, 0), komaSente KomaGinsho  )
  , ((7, 0), komaSente KomaKeima   )
  , ((8, 0), komaSente KomaKyosha  )
  , ((1, 1), komaSente KomaHisha   )
  , ((7, 1), komaSente KomaKakugyo )
  , ((0, 2), komaSente KomaFuhyo   )
  , ((1, 2), komaSente KomaFuhyo   )
  , ((2, 2), komaSente KomaFuhyo   )
  , ((3, 2), komaSente KomaFuhyo   )
  , ((4, 2), komaSente KomaFuhyo   )
  , ((5, 2), komaSente KomaFuhyo   )
  , ((6, 2), komaSente KomaFuhyo   )
  , ((7, 2), komaSente KomaFuhyo   )
  , ((8, 2), komaSente KomaFuhyo   )
  , ((0, 6), komaGote  KomaFuhyo   )
  , ((1, 6), komaGote  KomaFuhyo   )
  , ((2, 6), komaGote  KomaFuhyo   )
  , ((3, 6), komaGote  KomaFuhyo   )
  , ((4, 6), komaGote  KomaFuhyo   )
  , ((5, 6), komaGote  KomaFuhyo   )
  , ((6, 6), komaGote  KomaFuhyo   )
  , ((7, 6), komaGote  KomaFuhyo   )
  , ((8, 6), komaGote  KomaFuhyo   )
  , ((1, 7), komaGote  KomaKakugyo )
  , ((7, 7), komaGote  KomaHisha   )
  , ((0, 8), komaGote  KomaKyosha  )
  , ((1, 8), komaGote  KomaKeima   )
  , ((2, 8), komaGote  KomaGinsho  )
  , ((3, 8), komaGote  KomaKinsho  )
  , ((4, 8), komaGote  KomaOusho   )
  , ((5, 8), komaGote  KomaKinsho  )
  , ((6, 8), komaGote  KomaGinsho  )
  , ((7, 8), komaGote  KomaKeima   )
  , ((8, 8), komaGote  KomaKyosha  )
  ]
  where
    komaSente = Just . koma SentePlayer
    komaGote  = Just . koma GotePlayer

reverseIdx :: (Int, Int) -> (Int, Int)
reverseIdx (r, c) = (8 - r, 8 - c)

reverseIdxByPlayer :: StdShogiPlayer -> (Int, Int) -> (Int, Int)
reverseIdxByPlayer SentePlayer = id
reverseIdxByPlayer GotePlayer  = reverseIdx

canMove :: StdShogiPlayer -> (Int, Int) -> StdShogiComp -> Bool
canMove pid idx@(r, c) sc = isRange && isOwn
  where
    isRange = 0 <= r && r <= 8 && 0 <= c && c <= 8
    isOwn = maybe True not $ isOwnKoma pid idx sc

filterCanMove :: StdShogiPlayer -> StdShogiComp -> [(Int, Int)] -> [(Int, Int)]
filterCanMove pid sc = filter $ canMove pid `flip` sc

moveKomaByRange :: StdShogiPlayer -> (Int, Int) -> StdShogiComp -> [(Int, Int)] -> [(Int, Int)]
moveKomaByRange pid (r, c) sc rs = filterCanMove pid sc idxs
  where
    idxs = map biasIx rs
    
    biasIx :: (Int, Int) -> (Int, Int)
    biasIx idx = reverseIdxByPlayer pid idx & _1 %~ (+ r) & _2 %~ (+ c)

moveKomaByDir :: StdShogiPlayer -> (Int, Int) -> StdShogiComp -> (Int, Int) -> [(Int, Int)]
moveKomaByDir pid idx sc dir = idxsT ^. _1 ++ filterCanMove pid sc [head $ idxsT ^. _2] 
  where
    b = onboard sc
    
    idxsT = span canMove' $ tail $ iterate (addTuple dir) idx
    
    addTuple idx1 (r, c) = idx1 & _1 %~ (+ r) & _2 %~ (+ c)
    
    canMove' :: (Int, Int) -> Bool
    canMove' (-1, _ ) = False
    canMove' (_ , -1) = False
    canMove' (9 , _ ) = False
    canMove' (_ , 9 ) = False
    canMove' sidx = b ! sidx == Nothing

stdMoveKoma' :: StdShogiPlayer -> ShogiKoma -> (Int, Int) -> StdShogiComp -> [(Int, Int)]
stdMoveKoma' pid KomaFuhyo   idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [(0, 1)]

stdMoveKoma' pid KomaKyosha  idx sc = idxs
  where
    idxs = concat $ map canDirMove [(1, 0)]
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

stdMoveKoma' pid KomaOusho   idx sc = idxs
  where
    idxs = moveKomaByRange pid idx sc rs
    rs = [ (-1, -1), (0, -1), (1, -1)
         , (-1,  0), (0,  0), (1,  0)
         , (-1,  1), (0,  1), (1,  1)
         ]

stdMoveKoma' pid KomaHisha   idx sc = idxs
  where
    idxs = concat $ map canDirMove [(-1, 0), (0, -1), (1, 0), (0, 1)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma' pid KomaKakugyo idx sc = idxs
  where
    idxs = concat $ map canDirMove [(-1, -1), (1, -1), (1, 1), (-1, 1)]
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

stdMoveKoma' pid KomaRyuou   idx sc = idxsR ++ idxsD
  where
    idxsR = moveKomaByRange pid idx sc rs
    rs = [(-1, -1), (-1, 1), (1, 1), (1, -1)]
    
    idxsD = concat $ map canDirMove [(-1, 0), (0, -1), (1, 0), (0, 1)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma' pid KomaRyuma   idx sc = idxsR ++ idxsD
  where
    idxsR = moveKomaByRange pid idx sc rs
    rs = [(-1, 0), (0, -1), (1, 0), (0, 1)]
    
    idxsD = concat $ map canDirMove [(-1, -1), (1, -1), (1, 1), (-1, 1)]
    canDirMove = moveKomaByDir pid idx sc

stdMoveKoma :: StdShogiPlayer -> ShogiKoma -> (Int, Int) -> StdShogiComp -> [(Int, Int)]
stdMoveKoma pid sk idx sc = maybe [] id $ do
  bsk <- onboard sc ! idx
  return $ if koma pid sk == bsk
    then stdMoveKoma' pid sk idx sc
    else []

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
        , onhands = maybe ohs id $ do
          psk <- b ! idx2
          return $ insertOnHands pid (komaId psk) ohs
        }
      else Nothing
    
    b = onboard sc
    ohs = onhands sc
    
    skMoves = stdMoveKoma pid sk idx1 sc

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
canMoveOnBoard pid idx1 idx2 sk sc = (maybe False id $ do
  bsk <- b ! idx1
  let chKoma = canChKoma idx2 (komaId bsk) sk
  let ownKoma1 = pid == player bsk
  return $ chKoma && ownKoma1) &&
  (maybe True id $ do
  ownKoma2 <- isOwnKoma pid idx2 sc
  return $ not ownKoma2)
  where
    b = onboard sc
    
    canChKoma :: (Int, Int) -> ShogiKoma -> ShogiKoma -> Bool
    canChKoma idx ksk nsk = ksk == nsk ||
      canChRange pid idx && (maybe False id $ do
      bsk <- base nsk
      return $ ksk == bsk)
    
    canChRange :: StdShogiPlayer -> (Int, Int) -> Bool
    canChRange SentePlayer (_, x) = x >= 6
    canChRange GotePlayer  (_, x) = x <= 2

canMoveOnHands :: StdShogiPlayer -> (Int, Int) -> ShogiKoma -> StdShogiComp -> Bool
canMoveOnHands pid idx sk sc = ohc > 0 &&
  maybe True (const False) (b ! idx)
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
