module LibShogiCUI.ShogiCUI 
  ( printConsoleShogi
  ) where

import qualified System.Console.ANSI as ConsoleA ()
import           Control.Lens
import           Data.List

import LibShogi.Data.ShogiKoma
import LibShogi.Data.ShogiBoard
import LibShogi.Data.ShogiStd

-- showKoma = show :: (ShogiKoma -> String)
showKoma :: StdShogiPlayer -> ShogiKoma -> String
showKoma _           KomaFuhyo   = "歩"
showKoma _           KomaKyosha  = "香"
showKoma _           KomaKeima   = "桂"
showKoma _           KomaGinsho  = "銀"
showKoma _           KomaKinsho  = "金"
showKoma SentePlayer KomaOusho   = "王"
showKoma GotePlayer  KomaOusho   = "玉"
showKoma _           KomaHisha   = "飛"
showKoma _           KomaKakugyo = "角"
showKoma _           KomaTokin   = "と"
showKoma _           KomaNarikyo = "杏"
showKoma _           KomaNarikei = "圭"
showKoma _           KomaNarigin = "全"
showKoma _           KomaRyuou   = "龍"
showKoma _           KomaRyuma   = "馬"

showPlayer :: StdShogiPlayer -> String
showPlayer SentePlayer = "先手"
showPlayer GotePlayer  = "後手"

printConsoleShogi :: StdShogiComp -> IO ()
printConsoleShogi sc = printOnHands SentePlayer
  where
    b = onboard sc
    ohs = onhands sc
    
    printOnHands :: StdShogiPlayer -> IO ()
    printOnHands pid = do
      let xs = map (showKomaOnHand pid) $ assocsOnHands pid ohs
      let xss = map (^. _1) $ iterate (\(_, ys) -> splitAt 4 ys) ([], xs)
      let xssz = zip xss $ showPlayer pid:iterate id "    "
      foldl (\x s -> x >> putOnHandLine s) (putOnHandLine $ head xssz) $
        takeWhile (\(arr, _) -> arr /= []) $ tail xssz
    
    showKomaOnHand :: StdShogiPlayer -> (ShogiKoma, Int) -> String
    showKomaOnHand pid (k, i) = showKoma pid k ++ "x" ++ show i
    
    putOnHandLine :: ([String], String) -> IO ()
    putOnHandLine (xss, h) = do
      putStrLn $ h ++ ": " ++ intercalate ", " xss
