module LibLSSP.Senders.Base
  ( showVersionText
  , showTDataFormat
  , crlfText
  , withend
  , intertrans
  ) where

import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base

showTVersion :: Base.Version -> T.Text
showTVersion v = T.pack $ show v

showTDataFormat :: Base.DataFormatInfo -> T.Text
showTDataFormat df = name df
  `T.append` T.singleton '/'
  `T.append` (showTVersion $ version df)

crlfText :: T.Text
crlfText = T.pack "\r\n"

withend :: T.Text -> T.Text
withend s = s `T.append` crlfText

intertrans :: T.Text -> T.Text -> T.Text -> [T.Text] -> T.Text
intertrans f b c xs = f `T.append` intercalate c xs `T.append` b
