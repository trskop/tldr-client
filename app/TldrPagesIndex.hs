-- |
-- Module:      TldrPagesIndex
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module TldrPagesIndex
    (
    -- * Index
      Index(..)
    , Command(..)
    , Target(..)
    )
  where

import Control.Applicative ((<|>), pure)
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Aeson.Encoding as Aeson (text)
import Data.Text (Text)

import Locale (Locale(..))

-- | Represents @index.json@ of @tldr-pages@.
data Index = Index
    { commands :: [Command]
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Command = Command
    { name :: Text
    , platform :: NonEmpty Text
    , language :: NonEmpty SomeLocale
    , targets :: NonEmpty Target
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Target = Target
    { os :: Text
    , language :: SomeLocale
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SomeLocale
    = KnownLocale Locale
    | UnknownLocale Text
  deriving stock (Eq, Generic, Show)

instance FromJSON SomeLocale where
    parseJSON :: Aeson.Value -> Aeson.Parser SomeLocale
    parseJSON = Aeson.withText "SomeLocale" \t ->
        (KnownLocale <$> parseJSON (Aeson.String t)) <|> pure (UnknownLocale t)

instance ToJSON SomeLocale where
    toJSON :: SomeLocale -> Aeson.Value
    toJSON = \case
        KnownLocale l -> toJSON l
        UnknownLocale t -> Aeson.String t

    toEncoding :: SomeLocale -> Aeson.Encoding
    toEncoding = \case
        KnownLocale l -> toEncoding l
        UnknownLocale t -> Aeson.text t
