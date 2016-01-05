{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Parsers.Base
  ( lexeme
  , paramOr
  , version
  , dataFormat
  , seconds
  , lineValue
  , crlf
  , asciiLowerLetter
  , asciiUpperLetter
  , asciiLetter
  , asciiDigit
  , asciiAllowSym
  , asciiWSpace
  , uniDigit
  , uniLetter
  , uniMark
  , uniPunctuation
  , uniSymbol
  , uniFormat
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as AParsec
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.CharSet as CS
import qualified Data.CharSet.Unicode.Category as UCS

import qualified LibLSSP.Comps.Base as Base

lexeme :: AParsec.Parser a -> AParsec.Parser a
lexeme p = p <* AParsec.skipSpace

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

seconds :: AParsec.Parser Base.Time
seconds = Base.Seconds <$> AParsec.decimal

lineValue :: AParsec.Parser T.Text
lineValue = do
  x <- uniExcludeFmt
  xs <- AParsec.many' $ uniExcludeFmt <|> uniFormat
  let p = return $ T.pack $ x:xs
  p AParsec.<?> "line value"
  where
    uniExcludeFmt = uniDigit <|> uniLetter <|> uniMark <|> uniPunctuation <|> uniSymbol

crlf :: AParsec.Parser T.Text
crlf = AParsec.string "\r\n"

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

asciiWSpace :: AParsec.Parser Char
asciiWSpace = AParsec.satisfy isAsciiWSpace AParsec.<?> "ascii whitespace"
  where
    isAsciiWSpace :: Char -> Bool
    isAsciiWSpace ' '  = True
    isAsciiWSpace '\t' = True
    isAsciiWSpace _    = False

-- [Nd]
uniDigit :: AParsec.Parser Char
uniDigit = AParsec.satisfy isUniDigit AParsec.<?> "unicode digit"
  where
    isUniDigit :: Char -> Bool
    isUniDigit c = CS.member c UCS.decimalNumber

-- [Ll, Lu, Lt, Lm, Lo, Nl]
uniLetter :: AParsec.Parser Char
uniLetter = AParsec.satisfy isUniLetter AParsec.<?> "unicode letter"
  where
    isUniLetter :: Char -> Bool
    isUniLetter c = CS.member c $
      foldl1 CS.union
      [ UCS.lowercaseLetter
      , UCS.uppercaseLetter
      , UCS.titlecaseLetter
      , UCS.modifierLetter
      , UCS.otherLetter
      , UCS.letterNumber
      ]

-- [Mn, Mc]
uniMark :: AParsec.Parser Char
uniMark = AParsec.satisfy isUniMark AParsec.<?> "unicode mark"
  where
    isUniMark :: Char -> Bool
    isUniMark c = CS.member c $
      foldl1 CS.union
      [ UCS.nonSpacingMark
      , UCS.spacingCombiningMark
      ]

-- [Pd, Ps, Pe, Pi, Pf, Pc, Po]
uniPunctuation :: AParsec.Parser Char
uniPunctuation = AParsec.satisfy isUniPunctuation AParsec.<?> "unicode punctuation"
  where
    isUniPunctuation :: Char -> Bool
    isUniPunctuation c = CS.member c $
      foldl1 CS.union
      [ UCS.dashPunctuation
      , UCS.openPunctuation
      , UCS.closePunctuation
      , UCS.initialQuote
      , UCS.finalQuote
      , UCS.connectorPunctuation
      , UCS.otherPunctuation
      ]

-- [No, Sm, Sc, Sk, So]
uniSymbol :: AParsec.Parser Char
uniSymbol = AParsec.satisfy isUniSymbol AParsec.<?> "unicode symbol"
  where
    isUniSymbol :: Char -> Bool
    isUniSymbol c = CS.member c $
      foldl1 CS.union
      [ UCS.otherNumber
      , UCS.mathSymbol
      , UCS.currencySymbol
      , UCS.modifierSymbol
      , UCS.otherSymbol
      ]

-- [Zs, Cf, horizontal tab]
uniFormat :: AParsec.Parser Char
uniFormat = AParsec.satisfy isUniFormat AParsec.<?> "unicode format"
  where
    isUniFormat :: Char -> Bool
    isUniFormat c = CS.member c $
      foldl1 CS.union
      [ UCS.space
      , UCS.format
      , CS.singleton '\t'
      ]
