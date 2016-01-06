{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Parsers.RuleConsensus
  ( SetOptionsInfo (..)
  , ruleDeclaration
  , RuleCustomizeInfo (..)
  ) where

import           Control.Applicative
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AParsec

import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Comps.RuleConsensus
import qualified LibLSSP.Parsers.Base as PB

ruleMode :: AParsec.Parser RuleMode
ruleMode = declaration <|> customize
  where
    declaration :: AParsec.Parser RuleMode
    declaration = AParsec.string "declaration"
      *> return Declaration AParsec.<?> "declaration"
    
    customize :: AParsec.Parser RuleMode
    customize = AParsec.string "customize"
      *> return Customize AParsec.<?> "customize"

ruleDeclaration :: AParsec.Parser RuleDeclarationInfo
ruleDeclaration = RuleDeclarationInfo
  <$> ruleDeclarationName
  <*> (AParsec.char '/' *> PB.version)
  AParsec.<?> "rule declaration"
  where
    ruleDeclarationName = do
      x <- asciiLD
      xs <- AParsec.many' asciiLDS
      let p = return $ T.pack $ x:xs
      p AParsec.<?> "rule declaration name"
    
    asciiLD = PB.asciiLetter <|> PB.asciiDigit
    asciiLDS = asciiLD <|> PB.asciiAllowSym

consensusTime :: AParsec.Parser Base.Time
consensusTime = PB.seconds AParsec.<?> "consensus time"

ruleConsensus :: AParsec.Parser RuleConsensusInfo
ruleConsensus = agree <|> reject AParsec.<?> "rule consensus"
  where
    agree :: AParsec.Parser RuleConsensusInfo
    agree = AParsec.string "agree"
      *> return ConsensusAgree AParsec.<?> "agree"
    
    reject :: AParsec.Parser RuleConsensusInfo
    reject = AParsec.string "reject"
      *> return ConsensusReject AParsec.<?> "reject"
