{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , nextShogiComp
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Text            as AP
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Shogi.Board
import           Data.Shogi.Internal.Koma        as Koma
import           Data.Shogi.Koma
import           Data.Shogi.StdTypes
import qualified Data.Text                       as T
import           Data.TextIO
import           Data.TextIO.WebSocket
import           LibLSSP.Comps.Base
import           LibLSSP.Comps.GameCommunicate
import           LibLSSP.Comps.GameEnd
import           LibLSSP.Comps.RuleConsensus
import           LibLSSP.DataFormats.Json
import           LibLSSP.Parsers.Base
import           LibLSSP.Parsers.Connect
import           LibLSSP.Parsers.GameCommunicate
import           LibLSSP.Parsers.RuleConsensus
import           LibLSSP.Senders.Base
import           LibLSSP.Senders.Connect
import           LibLSSP.Senders.GameCommunicate
import           LibLSSP.Senders.GameEnd
import           LibLSSP.Senders.GameStart
import           LibLSSP.Senders.RuleConsensus
import           TextShow

parseMaybe :: AP.Parser a -> T.Text -> Maybe a
parseMaybe p t = AP.maybeResult $ AP.parse p t

commandPrefix :: AP.Parser T.Text
commandPrefix = AP.takeWhile (/= ':') <* lexeme (AP.char ':')

getCommandPrefix :: T.Text -> Maybe T.Text
getCommandPrefix tx = parseMaybe commandPrefix tx

getCPrefixM :: T.Text -> T.Text -> TextIO T.Text
getCPrefixM name tx = case getCommandPrefix tx of
  Just pref -> if pref == name
    then return pref
    else throwParseError tx >> return ""
  Nothing   -> throwParseError tx >> return ""

getCRLF :: TextIO T.Text
getCRLF = do
  x <- getCh
  xs <- if x == '\r' then getCRLF' else getCRLF
  return $ T.cons x xs
  where
    getCRLF' = do
      x <- getCh
      if x == '\n'
        then return $ T.singleton x
        else do
          xs <- getCRLF
          return $ T.cons x xs

jsonCommand :: ToJSON a => T.Text -> a -> T.Text
jsonCommand pref v = withend $ pref
  `T.append` ": <- LSSP-JSON/1.0.0\r\n"
  `T.append` encodeText v
  `T.append` "\r\n"

throwParseError :: T.Text -> TextIO ()
throwParseError tx = throwText $ "This is illegal command: " `T.append` tx

initialConC :: StdShogiComp -> InitialContext
initialConC scomp = InitialContext True board colors Map.empty
  where
    (board, colors) = unzip $ map (\i -> unzip $ map (lookupOnBoard' i) [1..9]) [1..9]

    lookupOnBoard' :: Int -> Int -> (Maybe GameKoma, Bool)
    lookupOnBoard' i j = case lookupOnBoard j i $ onboard scomp of
      Just (Koma.Koma pid kid) -> (Just $ convKid kid, convPid pid)
      Nothing                  -> (Nothing, False)

    convPid SentePlayer = True
    convPid GotePlayer  = False

    convKid = id

gameConC :: StdShogiComp -> GameContext
gameConC scomp = GameContext board colors Map.empty
  where
    (board, colors) = unzip $ map (\i -> unzip $ map (lookupOnBoard' i) [1..9]) [1..9]

    lookupOnBoard' :: Int -> Int -> (Maybe GameKoma, Bool)
    lookupOnBoard' i j = case lookupOnBoard j i $ onboard scomp of
      Just (Koma.Koma pid kid) -> (Just $ convKid kid, convPid pid)
      Nothing                  -> (Nothing, False)

    convPid SentePlayer = True
    convPid GotePlayer  = False

    convKid = id

gameStatC :: StdShogiComp -> GameStatusResult
gameStatC scomp = GameStatusResult board colors Map.empty
  where
    (board, colors) = unzip $ map (\i -> unzip $ map (lookupOnBoard' i) [1..9]) [1..9]

    lookupOnBoard' :: Int -> Int -> (Maybe GameKoma, Bool)
    lookupOnBoard' i j = case lookupOnBoard j i $ onboard scomp of
      Just (Koma.Koma pid kid) -> (Just $ convKid kid, convPid pid)
      Nothing                  -> (Nothing, False)

    convPid SentePlayer = True
    convPid GotePlayer  = False

    convKid = id

phase1 :: TextIO ()
phase1 = do
  pstr <- getTextLine
  tx <- getCPrefixM "Protocol" pstr
  putText $ allowDataFormatsCommand []
  pstr <- getTextLine
  tx <- getCPrefixM "Rule-Intension" pstr
  return ()

phase2 :: InitialContext -> TextIO ()
phase2 ic = do
  putText $ ruleModeCommand Declaration
  putText $ ruleDeclarationCommand $ RuleDeclarationInfo "standard" (Version3 1 0 0)
  putText $ jsonCommand "Initial-Context" ic
  pstr <- getTextLine
  tx <- getCPrefixM "Rule-Consensus" pstr
  return ()

phase3 :: TextIO ()
phase3 = do
  putText $ isReadyCommand "to game"
  pstr <- getTextLine
  tx <- getCPrefixM "Ready-Game" pstr
  return ()

phase4 :: TextIO ()
phase4 = putText $ gameStartCommand "good luck"

getActionM :: T.Text -> Maybe GameActionMoveInfo
getActionM = parseMaybe $ commandPrefix *> gameActionMove

convMoveInfo :: GameActionMoveInfo -> StdShogiMoveAction
convMoveInfo (MoveActionOnBoard idx1 idx2 k) = ShogiMoveOnBoard idx1 idx2 k
convMoveInfo (MoveActionOnHand  idx1      k) = ShogiMoveOnHand idx1 k

phase5 :: StdShogiComp -> Int -> TextIO ()
phase5 !gc !i = do
  putText $ jsonCommand "Game-Context" $ gameConC gc
  putText $ goCommand "thinking"
  pstr <- getTextLine
  tx <- getCPrefixM "Game-Action-Mode" pstr
  pstr <- getTextLine
  tx <- getCPrefixM "Game-Action-Move" pstr
  case getActionM pstr of
    Just act -> case move SentePlayer (convMoveInfo act) gc of
      Just ngc -> do
        putText $ gameStatusCommand GameContinue
        putText $ jsonCommand "Game-Status-Result" $ gameStatC ngc
        let (ni, nsc) = nextShogiComp i ngc
        phase5 nsc ni
      Nothing  -> do
        putText $ gameStatusCommand GameEnd
        putText $ jsonCommand "Game-Status-Result" $ gameStatC gc
        putText $ gameEndCommand GameEndLose
    Nothing  -> void $ throwParseError pstr

nextShogiComp :: Int -> StdShogiComp -> (Int, StdShogiComp)
nextShogiComp i sc = fromMaybe (nextShogiComp ni sc) $ do
  let r = i `mod` 9 + 1
  let c = i `div` 9 + 1
  (Koma.Koma pid k) <- lookupOnBoard r c board
  if pid == GotePlayer
    then do
      let nidx = head $ stdMoveKoma (r, c) sc
      nsc <- move GotePlayer (ShogiMoveOnBoard (r, c) nidx k) sc
      return (ni, nsc)
    else return $ nextShogiComp ((i + 11) `mod` 81) sc
  where
    board = onboard sc

    ni = i + 7 `mod` 81

serverIO :: TextIO ()
serverIO = do
  phase1
  phase2 $ initialConC stdShogiComp
  phase3
  phase4
  phase5 stdShogiComp 0

someFunc :: IO ()
someFunc = runTIOWSServer "0.0.0.0" 4000 serverIO
--someFunc = runTIOTCPServer (serverSettings 8888 "*") serverIO
