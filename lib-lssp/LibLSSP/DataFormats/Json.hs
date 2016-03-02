{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module LibLSSP.DataFormats.Json where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString    as ABParsec
import qualified Data.Attoparsec.Text          as AParsec
import qualified Data.Attoparsec.Types         as AParsecT
import qualified Data.Char                     as Char
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes)
import           Data.Shogi.Koma               as CB
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as EL
import qualified LibLSSP.Comps.Base            as CB
import qualified LibLSSP.Comps.GameCommunicate as CGC
import qualified LibLSSP.Comps.RuleConsensus   as CRC
import           LibLSSP.DataFormats.Base
import           TextShow

data LSSP_JSON = LSSP_JSON_1_0_0

dfName :: T.Text
dfName = "LSSP-JSON"

instance DataFormat LSSP_JSON where
  type DataStruct LSSP_JSON = Value
  info LSSP_JSON_1_0_0 = CB.DataFormatInfo dfName $ CB.Version3 1 0 0

instance DetailDataFormat LSSP_JSON where
  type DetailDataStruct LSSP_JSON = Value
  toDetail _ obj = result defaultDetail id $ fromJSON obj
  fromDetail _ = toJSON

instance ToDataFormat LSSP_JSON where
  toData _ = encodeText
  toDetailInfo _ = encodeText
  toDetailData _ = encodeText
  toSetOptions _ = encodeText
  toRuleCustomize _ = encodeText
  toInitialContext _ = encodeText
  toGameContext _ = encodeText
  toGameStatusResult _ = encodeText

instance FromDataFormat LSSP_JSON where
  parseData _ = jsonText
  parseDetailInfo _ = parseJSONText
  parseDetailData _ = jsonText
  parseSetOptions _ = parseJSONText
  parseRuleCustomize _ = parseJSONText
  parseInitialContext _ = parseJSONText
  parseGameContext _ = parseJSONText
  parseGameStatusResult _ = parseJSONText

result :: b -> (a -> b) -> Result a -> b
result x _ (Error _)   = x
result _ f (Success x) = f x

encodeText :: ToJSON a => a -> T.Text
encodeText v = TL.toStrict $ EL.decodeUtf8 $ encode v

jsonText :: AParsec.Parser Value
jsonText = aparseABParse json
  where
    aparseABParse :: ABParsec.Parser a -> AParsec.Parser a
    aparseABParse p = do
      AParsec.take 0
      aparseResult $ ABParsec.parse p ""

    aparseResult :: ABParsec.Result a -> AParsec.Parser a
    aparseResult (AParsecT.Fail _ []    _) = empty
    aparseResult (AParsecT.Fail _ (e:_) _) = fail e
    aparseResult (AParsecT.Done _ r)       = return r
    aparseResult (AParsecT.Partial f)      = do
      x <- AParsec.take 1
      aparseResult $ f $ E.encodeUtf8 x

parseJSONText :: forall a. FromJSON a => AParsec.Parser a
parseJSONText = do
  v <- jsonText
  aparseJSON v
  where
    aparseJSON :: Value -> AParsec.Parser a
    aparseJSON v = case fromJSON v of
      Error   s -> fail s
      Success a -> return a

paramOr :: Alternative f => (T.Text -> f a) -> T.Text -> f a
paramOr f s = f s <|> maybe empty f (conv s)
  where
    conv :: T.Text -> Maybe T.Text
    conv ss = case AParsec.parseOnly repUnderbar ss of
      Right x -> Just x
      _       -> Nothing

    repUnderbar = do
      xs <- AParsec.takeTill (== '_')
      xss <- AParsec.many' $ do
        y <- AParsec.char '_' *> AParsec.letter
        ys <- AParsec.takeTill (== '_')
        return $ T.cons (Char.toUpper y) ys
      return $ foldl T.append xs xss

elemJSON :: ToJSON a => T.Text -> a -> Maybe Pair
elemJSON k e = Just $ k .= e

elemMaybe :: ToJSON a => T.Text -> Maybe a -> Maybe Pair
elemMaybe k me = (k .=) <$> me

instance FromJSON CB.DetailInfo where
  parseJSON (Object v)
    = CB.DetailInfo
    <$> paramOr (v .:)  "detail_type"
    <*> paramOr (v .:?) "message"
    <*> paramOr (v .:?) "detail_message"
  parseJSON _ = empty

instance ToJSON CB.DetailInfo where
  toJSON v = object $ catMaybes
    [ "detail_type"    `elemJSON`  CB.detailType v
    , "message"        `elemMaybe` CB.message v
    , "detail_message" `elemMaybe` CB.detailMessage v
    ]

instance FromJSON CB.GameKoma where
  parseJSON (String str) = return $ case str of
      "FU" -> CB.KomaFuhyo
      "KY" -> CB.KomaKyosha
      "KE" -> CB.KomaKeima
      "GI" -> CB.KomaGinsho
      "KI" -> CB.KomaKinsho
      "OU" -> CB.KomaOsho
      "KA" -> CB.KomaKakugyo
      "HI" -> CB.KomaHisha
      "TO" -> CB.KomaTokin
      "NK" -> CB.KomaNarikyo
      "NI" -> CB.KomaNarikei
      "NG" -> CB.KomaNarigin
      "RO" -> CB.KomaRyuo
      "RM" -> CB.KomaRyuma
  parseJSON _ = empty

instance ToJSON CB.GameKoma where
  toJSON k = String $ case k of
    CB.KomaFuhyo   -> "FU"
    CB.KomaKyosha  -> "KY"
    CB.KomaKeima   -> "KE"
    CB.KomaGinsho  -> "GI"
    CB.KomaKinsho  -> "KI"
    CB.KomaOsho    -> "OU"
    CB.KomaKakugyo -> "KA"
    CB.KomaHisha   -> "HI"
    CB.KomaTokin   -> "TO"
    CB.KomaNarikyo -> "NK"
    CB.KomaNarikei -> "NI"
    CB.KomaNarigin -> "NG"
    CB.KomaRyuo    -> "RO"
    CB.KomaRyuma   -> "RM"

instance FromJSON (Maybe CB.GameKoma) where
  parseJSON v@(String str) = if str == ""
    then return Nothing
    else Just <$> parseJSON v

instance ToJSON (Maybe CB.GameKoma) where
  toJSON Nothing  = String ""
  toJSON (Just k) = toJSON k

instance FromJSON CRC.SetOptionsInfo where
  parseJSON (Object v)
    = CRC.SetOptionsInfo
    <$> paramOr (v .:) "rules"
  parseJSON _ = empty

instance ToJSON CRC.SetOptionsInfo where
  toJSON v = object
    [ "rules" .= CRC.rules v
    ]

instance FromJSON CRC.RuleCustomizeInfo where
  parseJSON (Object v)
    = CRC.RuleCustomizeInfo
    <$> paramOr (v .:) "is_waiting"
  parseJSON _ = empty

instance ToJSON CRC.RuleCustomizeInfo where
  toJSON v = object
    [ "is_waiting" .= CRC.isWaiting v
    ]

instance FromJSON CRC.InitialContext where
{-  parseJSON (Object v)
    = CRC.InitialContext
    <$> paramOr (v .:) "max_moves"-}
  parseJSON _ = empty

instance ToJSON CRC.InitialContext where
  toJSON v = object $ catMaybes
    [ "your_turn" `elemJSON`  CRC.yourTurn v
    , "board"     `elemJSON`  CRC.board    v
    , "colors"    `elemJSON`  CRC.colors   v
    , "hands"     `elemJSON`  Map.toAscList (CRC.hands v)
    ]

instance FromJSON CGC.GameContext where
{-  parseJSON (Object v)
    = CGC.GameContext
    <$> paramOr (v .:)  "your_turn"
    <*> paramOr (v .:?) "moves"
    <*> paramOr (v .:?) "max_moves"
    <*> paramOr (v .:)  "board"
    <*> paramOr (v .:)  "colors"
    <*> paramOr (v .:)  "hands"-}
  parseJSON _ = empty

instance ToJSON CGC.GameContext where
  toJSON v = object $ catMaybes
    [ "board"     `elemJSON`  CGC.board    v
    , "colors"    `elemJSON`  CGC.colors   v
    , "hands"     `elemJSON`  Map.toAscList (CGC.hands v)
    ]

instance FromJSON CGC.GameStatusResult where
  parseJSON _ = empty

instance ToJSON CGC.GameStatusResult where
  toJSON v = object $ catMaybes
    [ "board"     `elemJSON`  CGC.boardS    v
    , "colors"    `elemJSON`  CGC.colorsS   v
    , "hands"     `elemJSON`  Map.toAscList (CGC.handsS v)
    ]

