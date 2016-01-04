module LibLSSP.Parsers.Connect
  ( protocol
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as AParsec
import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Comps.Connect
import qualified LibLSSP.Parsers.Base as PB

protocol :: AParsec.Parser ProtocolInfo
protocol = ProtocolInfo
  <$> protocolName
  <*> (AParsec.char '/' *> PB.version)
  AParsec.<?> "protocol"
  where
    protocolName = do
      x <- asciiLD
      xs <- AParsec.many' asciiLDS
      let p = return $ T.pack $ x:xs
      p AParsec.<?> "protocol name"
    
    asciiLD = PB.asciiLetter <|> PB.asciiDigit
    asciiLDS = asciiLD <|> PB.asciiAllowSym

allowDataFormats :: AParsec.Parser [Base.DataFormatInfo]
allowDataFormats = AParsec.many' PB.dataFormat <|> return [defaultDataFormat]
  AParsec.<?> "allow data formats"
  where
    defaultDataFormat = Base.DataFormatInfo
      { name = "plain"
      , version = Base.Version3 1 0 0
      }
