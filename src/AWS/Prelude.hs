module AWS.Prelude
  ( module Exports
  , maybeToEither
  , safeHead
  , showc
  ) where

import Control.Applicative    as Exports (empty, pure)
import Control.Exception      as Exports (Exception, SomeException)
import Control.Monad          as Exports (forever, unless, void, when)
import Control.Monad.Catch    as Exports (MonadCatch, catch, throwM, try)
import Control.Monad.IO.Class as Exports (MonadIO, liftIO)
import Data.Bifunctor         as Exports
import Data.Conversions       as Exports
import Data.Map               as Exports (Map)
import Data.Text              as Exports (Text)
import Data.Text.Encoding     as Exports (decodeUtf8, encodeUtf8)
import Data.Time              as Exports (UTCTime)
import GHC.Generics           as Exports (Generic)

showc :: forall b a . (Show a, Conversion b String) => a -> b
showc = convert . show

safeHead :: [a] -> Maybe a
safeHead = \case
  (x:_) -> pure x
  _     -> empty

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b ma = case ma of
  Nothing -> Left b
  Just a  -> Right a
