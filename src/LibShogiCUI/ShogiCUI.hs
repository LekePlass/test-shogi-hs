module LibShogiCUI.ShogiCUI 
  ( printConsoleShogi
  ) where

import qualified System.Console.ANSI as ConsoleA

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

printConsoleShogi :: StdShogiComp -> IO ()
printConsoleShogi sc = 
  where
    b = onboard sc
    ohs = onhands sc
    
    printOnHands :: StdShogiPlayer -> IO ()
    printOnHands pid = assocsOnHands pid ohs
