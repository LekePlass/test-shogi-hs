{-# LANGUAGE OverloadedStrings #-}

module LibLSSP.Senders.Connect
  ( protocolCommand
  , allowDataFormatsCommand
  ) where

import qualified Data.Text as T

import qualified LibLSSP.Comps.Base as Base
import           LibLSSP.Comps.Connect
import           LibLSSP.Senders.Base

protocolCommand :: ProtocolInfo -> T.Text
protocolCommand pinfo = withend
  $          "Protocol: "
  `T.append` name pinfo
  `T.append` T.singleton '/'
  `T.append` (showTVersion $ version pinfo)

allowDataFormatsCommand :: [Base.DataFormatInfo] -> T.Text
allowDataFormatsCommand dfs = withend
  $          "Allow-Data-Formats: "
  `T.append` (T.intercalate " " $ map showTDataFormat dfs)
