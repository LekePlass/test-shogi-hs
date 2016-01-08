{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Senders.Base
  ( showTVersion
  , showTDataFormat
  , showTTime
  , crlfText
  , withend
  , intertrans
  ) where

import qualified Data.Text          as T

import qualified LibLSSP.Comps.Base as Base

-- TODO rewrite to Instance TextShow
showTVersion :: Base.Version -> T.Text
showTVersion v = T.pack $ show v

-- TODO rewrite to Instance TextShow
showTDataFormat :: Base.DataFormatInfo -> T.Text
showTDataFormat df = Base.name df
  `T.append` T.singleton '/'
  `T.append` showTVersion (Base.version df)

-- TODO rewrite to Instance TextShow
showTTime :: Base.Time -> T.Text
showTTime (Base.Seconds ti) = T.pack $ show ti

crlfText :: T.Text
crlfText = "\r\n"

withend :: T.Text -> T.Text
withend s = s `T.append` crlfText

intertrans :: T.Text -> T.Text -> T.Text -> [T.Text] -> T.Text
intertrans f b c xs = f `T.append` T.intercalate c xs `T.append` b
