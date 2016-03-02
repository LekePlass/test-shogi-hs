{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TextIO
  ( TextIO
  , putText
  , getCh
  , putTextLn
  , getTextLine
  , throwText
  ) where

import qualified Data.Text            as T
import           Data.TextIO.Internal
import           Prelude              hiding (take)

putTextLn :: T.Text -> TextIO ()
putTextLn txt = putText $ txt `T.append` "\n"

getTextLine :: TextIO T.Text
getTextLine = do
  str <- getTextLine'
  return $ T.pack str
  where
    getTextLine' :: TextIO String
    getTextLine' = do
      ch <- getCh
      if ch == '\n'
        then return []
        else do
          str <- getTextLine'
          return $ ch : str

take :: Int -> TextIO T.Text
take 0 = return ""
take n
  | n < 0     = error "Only non-negative values are allowed."
  | otherwise = do
    x <- getCh
    xs <- take (n - 1)
    return $ T.cons x xs
