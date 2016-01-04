module LibLSSP.Senders.RuleConsensus
  ( ruleIntensionCommand
  ) where

import qualified Data.Text as T

import LibLSSP.Comps.RuleConsensus
import LibLSSP.Senders.Base

ruleIntensionCommand :: T.Text -> T.Text
ruleIntensionCommand s = withend
  $          T.pack "Rule-Intension: "
  `T.append` s

ruleDeclarationCommand :: RuleMode -> T.Text
ruleDeclarationCommand Declaration = withend 
  $ T.pack "Rule-Mode: declaration"
ruleDeclarationCommand Customize   = withend 
  $ T.pack "Rule-Mode: customize"
