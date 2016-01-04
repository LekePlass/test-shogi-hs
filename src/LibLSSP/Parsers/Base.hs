module LibLSSP.Parsers.Base
  ( lexeme
  , version
  , asciiLowerLetter
  , asciiUpperLetter
  , asciiLetter
  , asciiDigit
  , asciiAllowSym
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as AParsec
import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base

lexeme :: AParsec.Parser a -> AParsec.Parser a
lexeme p = p <* AParsec.skipSpace

version :: AParsec.Parser Base.Version
version = maybeV *> (version3a <|> version3) AParsec.<?> "version"
  where
    maybeV = AParsec.char 'v' <|> return 'v'
    
    dotDecimal = AParsec.char '.' *> AParsec.decimal
    
    version3  = Base.Version3
      <$> AParsec.decimal
      <*> (dotDecimal <|> return 0)
      <*> (dotDecimal <* AParsec.many' dotDecimal <|> return 0)
    
    version3a = Base.Version3a
      <$> AParsec.decimal
      <*> (dotDecimal <|> return 0)
      <*> (dotDecimal <* AParsec.many' dotDecimal  <|> return 0)
      <*> asciiLowerLetter

dataFormat :: AParsec.Parser Base.DataFormatInfo
dataFormat = Base.DataFormatInfo
  <$> dataFormatName
  <*> (AParsec.char '/' *> version)
  AParsec.<?> "data format"
  where
    dataFormatName = do
      x <- asciiLD
      xs <- AParsec.many' asciiLDS
      let p = return $ T.pack $ x:xs
      p AParsec.<?> "data format name"
    
    asciiLD = asciiLetter <|> asciiDigit
    asciiLDS = asciiLD <|> asciiAllowSym

asciiLowerLetter :: AParsec.Parser Char
asciiLowerLetter = AParsec.satisfy isAsciiLowerLetter AParsec.<?> "ascii lower letter"
  where
    isAsciiLowerLetter :: Char -> Bool
    isAsciiLowerLetter c = 'a' <= c && c <= 'z'

asciiUpperLetter :: AParsec.Parser Char
asciiUpperLetter = AParsec.satisfy isAsciiUpperLetter AParsec.<?> "ascii upper letter"
  where
    isAsciiUpperLetter :: Char -> Bool
    isAsciiUpperLetter c = 'A' <= c && c <= 'Z'

asciiLetter :: AParsec.Parser Char
asciiLetter = asciiLowerLetter <|> asciiUpperLetter AParsec.<?> "ascii letter"

asciiDigit :: AParsec.Parser Char
asciiDigit = AParsec.satisfy isAsciiDigit AParsec.<?> "ascii digit"
  where
    isAsciiDigit :: Char -> Bool
    isAsciiDigit c = '0' <= c && c <= '9'

asciiAllowSym :: AParsec.Parser Char
asciiAllowSym = AParsec.satisfy isAsciiAllowSym AParsec.<?> "ascii allow symbol"
  where
    isAsciiAllowSym :: Char -> Bool
    isAsciiAllowSym ':'  = True
    isAsciiAllowSym ';'  = True
    isAsciiAllowSym '.'  = True
    isAsciiAllowSym ','  = True
    isAsciiAllowSym '_'  = True
    isAsciiAllowSym '@'  = True
    isAsciiAllowSym '+'  = True
    isAsciiAllowSym '\'' = True
    isAsciiAllowSym '<'  = True
    isAsciiAllowSym '>'  = True
    isAsciiAllowSym '{'  = True
    isAsciiAllowSym '}'  = True
    isAsciiAllowSym '['  = True
    isAsciiAllowSym ']'  = True
    isAsciiAllowSym '('  = True
    isAsciiAllowSym ')'  = True
    isAsciiAllowSym _    = False
