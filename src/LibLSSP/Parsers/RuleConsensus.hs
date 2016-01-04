module LibLSSP.Parsers.RuleConsensus
  ( SetOptionsInfo (..)
  , ruleDeclaration
  , RuleCustomizeInfo (..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AParsec

import           LibLSSP.Comps.RuleConsensus
import qualified LibLSSP.Parsers.Base as PB

instance FromJSON SetOptionsInfo where
  parseJSON (Object v)
    = SetOptionsInfo
    <$> v .: T.pack "rules"
  parseJSON _           = mzero

instance ToJSON SetOptionsInfo where
  toJSON v = object
    [ T.pack "rules" .= rules v
    ]

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

instance FromJSON RuleCustomizeInfo where
  parseJSON (Object v)
    = RuleCustomizeInfo
    <$> v .: T.pack "isWaiting"
  parseJSON _           = mzero

instance ToJSON RuleCustomizeInfo where
  toJSON v = object
    [ T.pack "isWaiting" .= isWaiting v
    ]

instance FromJSON InitialContext where
  parseJSON (Object v)
    = InitialContext
    <$> (v .:? T.pack "maxMoves" <|> v .:? T.pack "max_moves")
  parseJSON _           = mzero

instance ToJSON InitialContext where
  toJSON v = object $ catMaybes
    [ T.pack "max_moves" `maybeElem` maxMoves v
    ]
    where
      maybeElem k ma = (k .=) <$> ma

consensusTime :: AParsec.Parser Int
consensusTime = PB.seconds AParsec.<?> "consensus time"

ruleConsensus :: AParsec.Parser Bool
ruleConsensus = agree <|> reject
  where
    agree :: AParsec.Parser Bool
    agree = (AParsec.string $ T.pack "agree")
      *> return True AParsec.<?> "agree"
    
    reject :: AParsec.Parser Bool
    reject = (AParsec.string $ T.pack "reject")
      *> return False AParsec.<?> "reject"
