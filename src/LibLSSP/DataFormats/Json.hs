{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module LibLSSP.DataFormats.Json where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text as AParsec
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import           Data.Maybe (catMaybes)
import           Control.Applicative

import qualified LibLSSP.Comps.Base as CB
import qualified LibLSSP.Comps.RuleConsensus as CRC
import qualified LibLSSP.Comps.GameCommunicate as CGC
import           LibLSSP.DataFormats.Base

data LSSP_JSON = LSSP_JSON_1_0_0

dfName :: T.Text
dfName = "LSSP-JSON"

instance DataFormat LSSP_JSON where
  type DataStruct LSSP_JSON = Value
  info LSSP_JSON_1_0_0 = CB.DataFormatInfo dfName $ CB.Version3 1 0 0

instance DetailDataFormat LSSP_JSON where
  toDetail _ obj = result defaultDetail id $ fromJSON obj
  fromDetail _ dinfo = toJSON dinfo

result :: b -> (a -> b) -> Result a -> b
result x _ (Error _)   = x
result _ f (Success x) = f x

encodeText :: ToJSON a => a -> T.Text
encodeText v = TL.toStrict $ EL.decodeUtf8 $ encode v

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
  parseJSON (Object v)
    = CRC.InitialContext
    <$> paramOr (v .:) "max_moves"
  parseJSON _ = empty

instance ToJSON CRC.InitialContext where
  toJSON v = object $ catMaybes
    [ "max_moves" `elemMaybe` CRC.maxMoves v
    ]

instance FromJSON CGC.GameContext where
  parseJSON (Object v)
    = CGC.GameContext
    <$> paramOr (v .:) "max_moves"
  parseJSON _ = empty

instance ToJSON CGC.GameContext where
  toJSON v = object $ catMaybes
    [ "max_moves" `elemMaybe` CGC.maxMoves v
    ]
