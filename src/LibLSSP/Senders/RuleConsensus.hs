{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Senders.RuleConsensus
  ( ruleIntensionCommand
  , ruleModeCommand
  , ruleDeclarationCommand
  , dataFormatCommand
  , consensusTimeCommand
  , ruleConsensusCommand
  ) where

import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Comps.RuleConsensus
import           LibLSSP.Senders.Base

ruleIntensionCommand :: T.Text -> T.Text
ruleIntensionCommand s = withend $ "Rule-Intension: " `T.append` s

ruleModeCommand :: RuleMode -> T.Text
ruleModeCommand Declaration = withend $ "Rule-Mode: declaration"
ruleModeCommand Customize   = withend $ "Rule-Mode: customize"

ruleDeclarationCommand :: RuleDeclarationInfo -> T.Text
ruleDeclarationCommand rdinfo = withend
  $          "Rule-Declaration: "
  `T.append` name rdinfo
  `T.append` T.singleton '/'
  `T.append` (showTVersion $ version rdinfo)

dataFormatCommand :: Base.DataFormatInfo -> T.Text
dataFormatCommand df = withend $ "Data-Format?: " `T.append` showTDataFormat df

consensusTimeCommand :: Base.Time -> T.Text
consensusTimeCommand ct = withend $ "Consensus-Time?: " `T.append` showTTime ct

ruleConsensusCommand :: RuleConsensusInfo -> T.Text
ruleConsensusCommand ConsensusAgree  = withend "Rule-Consensus: agree"
ruleConsensusCommand ConsensusReject = withend "Rule-Consensus: reject"
