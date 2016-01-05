{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.DataFormats.Base
  ( 
  ) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AParsec

import qualified LibLSSP.Comps.Base as CB
import qualified LibLSSP.Comps.RuleConsensus as CRC
import qualified LibLSSP.Comps.GameCommunicate as CGC

class DataFormat df where
  type DataStruct df
  info :: df -> CB.DataFormatInfo

class DetailDataFormat df where
  type DetailDataStruct df :: *
  type instance DetailDataStruct df = DataStruct df
  toDetail :: df -> (DetailDataStruct df) -> CB.DetailInfo
  fromDetail :: df -> CB.DetailInfo -> (DetailDataStruct df)

class DetailDataFormat df => ToDataFormat df where
  toData :: df -> (DataStruct df) -> T.Text
  
  toDetailInfo :: df -> CB.DetailInfo -> T.Text
  toDetailInfo dfmt = toDetailData dfmt . fromDetail dfmt
  
  toDetailData :: df -> (DetailDataStruct df) -> T.Text
  toDetailData dfmt = toDetailInfo dfmt . toDetail dfmt
  
  toSetOptions :: df -> CRC.SetOptionsInfo -> T.Text
  
  toRuleCustomize :: df -> CRC.RuleCustomizeInfo -> T.Text
  
  toInitialContext :: df -> CRC.InitialContext -> T.Text
  
  toRuleConsensusDetail :: df -> (DetailDataStruct df) -> T.Text
  toRuleConsensusDetail = toDetailData
  
  toGameContext :: df -> CGC.GameContext -> T.Text
  
  toGameActionDetail :: df -> (DetailDataStruct df) -> T.Text
  toGameActionDetail = toDetailData
  
  toGameStopDetail :: df -> (DetailDataStruct df) -> T.Text
  toGameStopDetail = toDetailData
  
  toGameStatusResult :: df -> CGC.GameStatusResult -> T.Text
  
  toGameEndDetail :: df -> (DetailDataStruct df) -> T.Text
  toGameEndDetail = toDetailData

class DetailDataFormat df => FromDataFormat df where
  parseData :: df -> AParsec.Parser (DataStruct df)
  
  parseDetailInfo :: df -> AParsec.Parser CB.DetailInfo
  
  parseDetailData :: df -> AParsec.Parser (DetailDataStruct df)
  
  parseSetOptions :: df -> AParsec.Parser CRC.SetOptionsInfo
  
  parseRuleCustomize :: df -> AParsec.Parser CRC.RuleCustomizeInfo
  
  parseInitialContext :: df -> AParsec.Parser CRC.InitialContext
  
  parseRuleConsensusDetail :: df -> AParsec.Parser (DetailDataStruct df)
  
  parseGameContext :: df -> AParsec.Parser CGC.GameContext
  
  parseGameActionDetail :: df -> AParsec.Parser (DetailDataStruct df)
  
  parseGameStopDetail :: df -> AParsec.Parser (DetailDataStruct df)
  
  parseGameStatusResult :: df -> AParsec.Parser CGC.GameStatusResult
  
  parseGameEndDetail :: df -> AParsec.Parser (DetailDataStruct df)

