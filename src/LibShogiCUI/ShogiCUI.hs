module LibShogiCUI.ShogiCUI 
  ( ConsoleColor (..)
  , color
  , colorText
  , showKoma
  , TblBorder (..)
  , showBorder
  , showPlayer
  , showEmNumber
  , showJaNumber
  , printConsoleShogi
  , getConsoleMove
  , game
  ) where

import           Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import           Control.Lens
import qualified System.Console.ANSI as CA

import LibShogi.Data.Koma
import LibShogi.Data.ShogiKoma
import LibShogi.Data.ShogiBoard
import LibShogi.Data.ShogiStd

import qualified LibShogiCUI.ShogiParseUtils as SPUs

type PlayerColorMap = Map.Map StdShogiPlayer (ConsoleColor, ConsoleColor)

data ConsoleColor
  = ColorBlack
  | ColorGray
  | ColorDarkRed
  | ColorRed
  | ColorDarkGreen
  | ColorGreen
  | ColorBrown
  | ColorYellow
  | ColorDarkBlue
  | ColorBlue
  | ColorPurple
  | ColorMagenta
  | ColorDarkCyan
  | ColorCyan
  | ColorWhite
  | ColorBrightWhite
  deriving ( Eq, Ord, Show )

color :: ConsoleColor -> (CA.ColorIntensity, CA.Color)
color ColorBlack       = (CA.Dull , CA.Black  )
color ColorGray        = (CA.Vivid, CA.Black  )
color ColorDarkRed     = (CA.Dull , CA.Red    )
color ColorRed         = (CA.Vivid, CA.Red    )
color ColorDarkGreen   = (CA.Dull , CA.Green  )
color ColorGreen       = (CA.Vivid, CA.Green  )
color ColorBrown       = (CA.Dull , CA.Yellow )
color ColorYellow      = (CA.Vivid, CA.Yellow )
color ColorDarkBlue    = (CA.Dull , CA.Blue   )
color ColorBlue        = (CA.Vivid, CA.Blue   )
color ColorPurple      = (CA.Dull , CA.Magenta)
color ColorMagenta     = (CA.Vivid, CA.Magenta)
color ColorDarkCyan    = (CA.Dull , CA.Cyan   )
color ColorCyan        = (CA.Vivid, CA.Cyan   )
color ColorWhite       = (CA.Dull , CA.White  )
color ColorBrightWhite = (CA.Vivid, CA.White  )

colorText :: ConsoleColor -> T.Text -> T.Text
colorText c s = packSGRCode [color2ForeSGR $ color c] `T.append`
  s `T.append` packSGRCode [CA.Reset]
  where
    color2ForeSGR :: (CA.ColorIntensity, CA.Color) -> CA.SGR
    color2ForeSGR (ci, cl) = CA.SetColor CA.Foreground ci cl
    
    packSGRCode :: [CA.SGR] -> T.Text
    packSGRCode = T.pack . CA.setSGRCode

-- showKoma = show :: (ShogiKoma -> String)
showKoma :: StdShogiPlayer -> ShogiKoma -> T.Text
showKoma _           KomaFuhyo   = T.pack "歩"
showKoma _           KomaKyosha  = T.pack "香"
showKoma _           KomaKeima   = T.pack "桂"
showKoma _           KomaGinsho  = T.pack "銀"
showKoma _           KomaKinsho  = T.pack "金"
showKoma SentePlayer KomaOsho    = T.pack "王"
showKoma GotePlayer  KomaOsho    = T.pack "玉"
showKoma _           KomaHisha   = T.pack "飛"
showKoma _           KomaKakugyo = T.pack "角"
showKoma _           KomaTokin   = T.pack "と"
showKoma _           KomaNarikyo = T.pack "杏"
showKoma _           KomaNarikei = T.pack "圭"
showKoma _           KomaNarigin = T.pack "全"
showKoma _           KomaRyuo    = T.pack "龍"
showKoma _           KomaRyuma   = T.pack "馬"

showConsoleKoma :: PlayerColorMap -> Maybe (ShogiBoardKoma StdShogiPlayer) -> T.Text
showConsoleKoma _  Nothing  = T.pack "　"
showConsoleKoma pm (Just k) = colorText c $ showKoma pid kid
  where
    pid = player k
    kid = komaId k
    cs  = maybe (ColorBlack, ColorDarkRed) id $ Map.lookup pid pm
    c   = cs ^. if isNarikoma kid then _2 else _1

data TblBorder
  = BorderLeftTop
  | BorderRightTop
  | BorderLeftBottom
  | BorderRightBottom
  | BorderCenter
  | BorderSide
  | BorderLength
  | BorderCenterLeft
  | BorderCenterRight
  | BorderCenterTop
  | BorderCenterBottom

showBorder :: TblBorder -> T.Text
showBorder BorderLeftTop      = T.pack "┌"
showBorder BorderRightTop     = T.pack "┐"
showBorder BorderLeftBottom   = T.pack "└"
showBorder BorderRightBottom  = T.pack "┘"
showBorder BorderCenter       = T.pack "┼"
showBorder BorderSide         = T.pack "─"
showBorder BorderLength       = T.pack "│"
showBorder BorderCenterTop    = T.pack "┬"
showBorder BorderCenterBottom = T.pack "┴"
showBorder BorderCenterLeft   = T.pack "├"
showBorder BorderCenterRight  = T.pack "┤"

showPlayer :: StdShogiPlayer -> T.Text
showPlayer SentePlayer = T.pack "先手"
showPlayer GotePlayer  = T.pack "後手"

showEmNumber :: Int -> T.Text
showEmNumber 0 = T.pack "０"
showEmNumber 1 = T.pack "１"
showEmNumber 2 = T.pack "２"
showEmNumber 3 = T.pack "３"
showEmNumber 4 = T.pack "４"
showEmNumber 5 = T.pack "５"
showEmNumber 6 = T.pack "６"
showEmNumber 7 = T.pack "７"
showEmNumber 8 = T.pack "８"
showEmNumber 9 = T.pack "９"
showEmNumber x
  | x >= 0     = showEmNumber (x `div` 10) `T.append` showEmNumber (x `mod` 10)
  | otherwise  = T.pack "ー" `T.append` showEmNumber (-x) 

showJaNumber :: Int -> T.Text
showJaNumber 0 = T.pack "〇"
showJaNumber 1 = T.pack "一"
showJaNumber 2 = T.pack "二"
showJaNumber 3 = T.pack "三"
showJaNumber 4 = T.pack "四"
showJaNumber 5 = T.pack "五"
showJaNumber 6 = T.pack "六"
showJaNumber 7 = T.pack "七"
showJaNumber 8 = T.pack "八"
showJaNumber 9 = T.pack "九"
showJaNumber x
  | x >= 0     = showJaNumber (x `div` 10) `T.append` showJaNumber (x `mod` 10)
  | otherwise  = T.pack "マイナス" `T.append` showJaNumber (-x)

intertrans :: [a] -> [a] -> [a] -> [[a]] -> [a]
intertrans f b c xss = f ++ intercalate c xss ++ b

intertransText :: T.Text -> T.Text -> T.Text -> [T.Text] -> T.Text
intertransText f b _ [] = f `T.append` b
intertransText f b c xs = f `T.append`
  foldl1 (\x y -> x `T.append` c `T.append` y) xs `T.append` b

printConsoleShogi :: StdShogiComp -> IO ()
printConsoleShogi sc = do
  printOnHand ColorBlack SentePlayer
  putStrLn ""
  printOnBoard $ Map.fromList $
    [ (SentePlayer, (ColorBlack, ColorDarkRed))
    , (GotePlayer , (ColorGray , ColorRed    ))
    ]
  putStrLn ""
  printOnHand ColorGray GotePlayer
  where
    bo = onboard sc
    ohs = onhands sc
    
    printOnHand :: ConsoleColor -> StdShogiPlayer -> IO ()
    printOnHand cc pid = do
      let xs = map (showKomaOnHand cc pid) $ assocsOnHands pid ohs
      let xss = [ x | (x, _) <- iterate (\(_, ys) -> splitAt 4 ys) ([], xs) ]
      let xssz = zip xss $ showPlayer pid:iterate id (T.pack "    ")
      foldl (\x s -> x >> putOnHandLine s) (putOnHandLine $ head xssz) $
        takeWhile (\(arr, _) -> arr /= []) $ tail xssz
    
    showKomaOnHand :: ConsoleColor -> StdShogiPlayer -> (ShogiKoma, Int) -> T.Text
    showKomaOnHand cc pid (k, i) = colorText cc (showKoma pid k) `T.append` T.pack "x" `T.append` SPUs.showText i
    
    putOnHandLine :: ([T.Text], T.Text) -> IO ()
    putOnHandLine (xss, h) = do
      TextIO.putStrLn $ h `T.append` T.pack ": " `T.append`
        intertransText SPUs.noText SPUs.noText (T.pack ", ") xss
    
    printOnBoard :: PlayerColorMap -> IO ()
    printOnBoard m = do
      foldl (\x s -> x >> TextIO.putStrLn s) (TextIO.putStrLn emNumLine) $
        intertrans [
          borderLineByData topBorders] [
          borderLineByData bottomBorders] [
          borderLineByData centerBorders] $
        [[showKomaLine m h `T.append` T.pack " " `T.append` showJaNumber h] | h <- [1..9]]
    
    showKomaLine :: PlayerColorMap -> Int -> T.Text
    showKomaLine m h = intertransText showLengthLine showLengthLine showLengthLine $
      [showConsoleKoma m $ lookupOnBoard (10 - w) h bo | w <- [1..9] ]
    
    showLengthLine = showBorder BorderLength
    
    emNumLine = intertransText (T.pack " ") SPUs.noText (T.pack " ") $ 
      [ showEmNumber s | s <- take 9 $ iterate (subtract 1) 9]
    
    borderLine :: T.Text -> T.Text -> T.Text -> T.Text
    borderLine f b c = intertransText f b c $
      take 9 $ iterate id $ foldl1 T.append $
      map showBorder $ take 2 $ iterate id BorderSide
    
    borderLineByData :: (TblBorder, TblBorder, TblBorder) -> T.Text
    borderLineByData (f, c, b) = borderLine (showBorder f) (showBorder b) (showBorder c)
    
    topBorders = (BorderLeftTop, BorderCenterTop, BorderRightTop)
    bottomBorders = (BorderLeftBottom, BorderCenterBottom, BorderRightBottom)
    centerBorders = (BorderCenterLeft, BorderCenter, BorderCenterRight)

-- TODO: rewrite using State
getConsoleMove :: T.Text -> IO (Maybe SPUs.ConsoleShogiMoveAction)
getConsoleMove s = do
  let loop i | i < 5 = do
        ans <- askAction
        let res = SPUs.parseMoveAction $ T.pack ans
        if res /= Nothing then
          return res
        else
          do
            printWarning
            loop $ i + 1
      loop _ = do
        putStrLn "プログラムを終了します"
        return Nothing
  loop 0
  where
    askAction = do
      TextIO.putStr $ s `T.append` T.pack "> "
      x <- getLine
      return x
    
    printWarning = do
      putStrLn "不正な入力です:"
      putStrLn " - (Int, Int); (Int, Int)[; Koma]"
      putStrLn " - (Int, Int); Koma"

-- TODO: rewrite using State
game :: StdShogiComp -> IO ()
game isc = do
  let loop i sc | i `mod` 2 == 0 = do
        printConsoleShogi sc
        putStrLn ""
        let pid = SentePlayer
        ans <- getConsoleMove $ showPlayer pid
        case ans of
          Nothing   -> return ()
          Just cact -> case moveSCKoma pid cact sc of
            Nothing  -> do
              putStrLn "動きが不正です"
              putStrLn ""
              loop i sc
            Just nsc -> do
              putStrLn ""
              loop (i + 1) nsc
      loop i sc = do
        let pid = GotePlayer
        let f idx@(r, c) = (idx, stdMoveKoma idx sc, lookupOnBoard r c $ onboard sc)
        let mss = map f [ (x, y) | x <- [1..9], y <- [1..9] ]
        let isOwn msk = maybe True (\sk -> player sk /= pid) msk
        let ms = head $ dropWhile (\(_, xs, msk) -> xs == [] || isOwn msk) mss
        case move pid (ShogiMoveOnBoard (ms ^. _1) (head $ ms ^. _2) (maybe KomaFuhyo komaId $ ms ^. _3)) sc of
          Just nsc -> loop (i + 1) nsc
          _        -> do
            putStrLn $ show ms
            return ()
  loop 0 isc
  where
    moveSCKoma :: StdShogiPlayer -> SPUs.ConsoleShogiMoveAction -> StdShogiComp -> Maybe StdShogiComp
    moveSCKoma pid cact sc = do
      act <- convCR cact sc
      move pid act sc
    
    convCR :: SPUs.ConsoleShogiMoveAction -> StdShogiComp -> Maybe StdShogiMoveAction
    convCR (SPUs.CSActionOnBoard (r, c) idx2 Nothing) sc = do
      sk <- lookupOnBoard r c $ onboard sc
      return $ ShogiMoveOnBoard (r, c) idx2 $ komaId sk
    convCR (SPUs.CSActionOnBoard idx1 idx2 (Just sk)) _  = Just $ ShogiMoveOnBoard idx1 idx2 sk
    convCR (SPUs.CSActionOnHand idx sk)               _  = Just $ ShogiMoveOnHand idx sk