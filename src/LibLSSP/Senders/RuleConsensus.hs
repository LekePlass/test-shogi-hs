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
ruleIntensionCommand s = withend
  $ T.pack "Rule-Intension: " `T.append` s

ruleModeCommand :: RuleMode -> T.Text
ruleModeCommand Declaration = withend 
  $ T.pack "Rule-Mode: declaration"
ruleModeCommand Customize   = withend 
  $ T.pack "Rule-Mode: customize"

ruleDeclarationCommand :: RuleDeclarationInfo -> T.Text
ruleDeclarationCommand rdinfo = withend
  $          T.pack "Rule-Declaration: "
  `T.append` name rdinfo
  `T.append` T.singleton '/'
  `T.append` (showTVersion $ version rdinfo)

dataFormatCommand :: Base.DataFormatInfo -> T.Text
dataFormatCommand df = withend
  $          T.pack "Data-Format?: "
  `T.append` showTDataFormat df

consensusTimeCommand :: Base.Time -> T.Text
consensusTimeCommand ct = withend
  $          T.pack "Consensus-Time?: "
  `T.append` showTTime ct

ruleConsensusCommand :: RuleConsensusInfo -> T.Text
ruleConsensusCommand ConsensusAgree  = withend
  $ T.pack "Rule-Consensus: agree"
ruleConsensusCommand ConsensusReject = withend
  $ T.pack "Rule-Consensus: reject"
