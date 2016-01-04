module LibLSSP.Senders.Connect
  ( protocolCommand
  , allowDataFormatsCommand
  ) where

import Data.Text as T

import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Comps.Connect
import           LibLSSP.Senders.Base

protocolCommand :: ProtocolInfo -> T.Text
protocolCommand pinfo = withend
  $          T.pack "Protocol: "
  `T.append` name pinfo
  `T.append` T.singleton '/'
  `T.append` (showTVersion $ version pinfo)

allowDataFormatsCommand :: [Base.DataFormatInfo] -> T.Text
allowDataFormatsCommand dfs = withend
  $          T.pack "Allow-Data-Formats: "
  `T.append` map Base.showTDataFormat dfs
