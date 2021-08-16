-- |
-- Module:      Configuration
-- Description: Configuration data type and Dhall decoding for it
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Configuration data type and Dhall decoding for it.
module Configuration
    ( Configuration(..)
    , Source(..)
    , SourceLocation(..)
    , SourceFormat(..)
    , decodeConfiguration
    , mkDefConfiguration
    , getCacheDirectory
    , getLocales
    )
  where

import Control.Applicative ((<|>), pure)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Semigroup ((<>))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Show (Show)

import qualified Data.LanguageCodes as LanguageCode (ISO639_1(EN))
import Data.Output.Colour (ColourOutput)
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import Dhall (FromDhall)
import qualified Dhall
    ( Decoder
    , auto
    , constructor
    , field
    , list
    , maybe
    , record
    , string
    , union
    )
import System.Directory (XdgDirectory(XdgCache), getXdgDirectory)

import Locale (Locale(Locale, country, language), decodeLocale, lookupLang)


data Configuration = Configuration
    { verbosity :: Verbosity
    -- ^ How verbose the application should be by default.
    , colourOutput :: ColourOutput
    -- ^ Should colours be used on terminal by the application?
    , cacheDirectory :: Maybe FilePath
    -- ^ `Nothing` means that it will use the following directory:
    --
    -- > ${XDG_CACHE_HOME:-${HOME}/.cache}/tldr
    --
    -- Be aware that the value is evaluated at the time of execution.
    , locale :: Maybe Locale
    -- ^ `Nothing` means that `LANG` environment variable needs to be used at
    -- the time of execution.
    , sources :: NonEmpty Source
    }
  deriving stock (Eq, Show)

decodeConfiguration :: Dhall.Decoder Configuration
decodeConfiguration = Dhall.record do
    verbosity <- Dhall.field "verbosity" Dhall.auto
    colourOutput <- Dhall.field "colourOutput" Dhall.auto
    cacheDirectory <- Dhall.field "cacheDirectory" Dhall.auto
    locale <- Dhall.field "locale" (Dhall.maybe decodeLocale)
    sources <- Dhall.field "sources" (decodeNonEmpty decodeSource)
    pure Configuration{..}

data Source = Source
    { name :: Text
    , format :: SourceFormat
    , location :: SourceLocation
    }
  deriving stock (Eq, Show)

decodeSource :: Dhall.Decoder Source
decodeSource = Dhall.record do
    name <- Dhall.field "name" Dhall.auto
    format <- Dhall.field "format" Dhall.auto
    location <- Dhall.field "location" decodeSourceLocation
    pure Source{..}

data SourceLocation
    = Local FilePath
    -- ^ The location can be either a directory or an archive.
    | Remote (NonEmpty String)
    -- ^ URLs to an archive.
  deriving stock (Eq, Generic, Show)

decodeSourceLocation :: Dhall.Decoder SourceLocation
decodeSourceLocation = Dhall.union
  (  (Local <$> Dhall.constructor "Local" Dhall.string)
  <> (Remote <$> Dhall.constructor "Remote" (decodeNonEmpty Dhall.string))
  )

data SourceFormat
    = TldrPagesWithIndex
    -- ^ We will be looking for @index.json@ file instead of traversing the
    -- whole directory structure.
    | TldrPagesWithoutIndex -- TODO: Should there be parameters for indexing?
    -- ^ Client will traverse the whole directory structure to build an index.
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromDhall)

decodeNonEmpty :: Dhall.Decoder a -> Dhall.Decoder (NonEmpty a)
decodeNonEmpty decoder = Dhall.record do
    head <- Dhall.field "head" decoder
    tail <- Dhall.field "tail" (Dhall.list decoder)
    pure (head :| tail)

-- | User's cache directory for pages. Be aware that the returned file path may
-- not exist.
getCacheDirectory :: Configuration -> IO FilePath
getCacheDirectory Configuration{cacheDirectory} =
    maybe (getXdgDirectory XdgCache "tldr-client") pure cacheDirectory

getLocales :: Configuration -> Maybe Locale -> IO [Locale]
getLocales Configuration{verbosity, locale} override =
    -- TODO: This doesn't follow the specification yet!
    maybe (pure <$> lookupLang verbosity defaultLocale) (pure . pure)
        (override <|> locale)
  where
    defaultLocale = Locale
        { language = LanguageCode.EN
        , country = Nothing
        }

mkDefConfiguration :: IO Configuration
mkDefConfiguration = do
    pure Configuration
        { verbosity = Verbosity.Normal
        , colourOutput = ColourOutput.Auto
        , cacheDirectory = Nothing
        , locale = Nothing
        , sources = pure Source
            { name = "tldr-pages"
            , format = TldrPagesWithIndex
            , location = Remote (assetsUrl1 :| [assetsUrl2])
            }
        }
  where
    -- URLs are taken from:
    -- <https://github.com/tldr-pages/tldr/blob/main/CLIENT-SPECIFICATION.md>
    --
    -- The `assetsUrl1` should be pointing to `assetsUrl2`. There are reasons
    -- why downloading the first one may fail while downloading from the second
    -- location may succeed. These include things like missconfiguration (on
    -- the side of tldr-pages) or DNS resolution issue.
    assetsUrl1 = "https://tldr.sh/assets/tldr.zip"
    assetsUrl2 =
      "https://raw.githubusercontent.com/tldr-pages/tldr-pages.github.io/master/assets/tldr.zip"
