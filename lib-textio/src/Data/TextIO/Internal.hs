{-# LANGUAGE GADTs #-}

module Data.TextIO.Internal
  ( TextIOOpe(..)
  , TextIO
  , putText
  , getCh
  , throwText
  ) where

import           Control.Monad.Catch
import           Control.Monad.Operational
import qualified Data.Text                 as T

data TextIOOpe a where
  PutText :: T.Text -> TextIOOpe ()
  GetCh   :: TextIOOpe Char
  Throw   :: T.Text -> TextIOOpe ()

type TextIO a = Program TextIOOpe a

putText :: T.Text -> TextIO ()
putText = singleton . PutText

getCh :: TextIO Char
getCh = singleton GetCh

instance Exception T.Text

throwText :: T.Text -> TextIO ()
throwText = singleton . Throw
