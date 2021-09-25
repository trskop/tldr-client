-- |
-- Module:      TldrClient.Configuration
-- Description: Configuration data type and Dhall decoding for it
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Configuration data type and Dhall decoding for it.
module TldrClient.Configuration
    ( Configuration(..)
    , Source(..)
    , SourceLocation(..)
    , SourceFormat(..)
    , decodeStandaloneConfiguration
    , decodeSubcommandConfiguration
    , mkDefConfiguration
    , getCacheDirectory
    , getLocales
    , getUseColours
    , shouldUseColours
    -- * Messages
    , putDebugLn
    , putWarningLn
    , putErrorLn
    )
  where

import Control.Applicative ((<|>), pure)
import Control.Monad (when)
import Data.Bool (Bool(False))
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.String (String)
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import System.IO
    ( FilePath
    , Handle
    , IO
    , hIsTerminalDevice
    , hPutStrLn
    , stderr
    )
import Text.Show (Show)

import Data.Output.Colour
    ( ColourOutput
    , terminalSupportsColours
    , useColoursWhen
    )
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Annoying, Normal))
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
import System.Console.Terminfo (setupTermFromEnv)
import System.Directory (XdgDirectory(XdgCache), getXdgDirectory)
import System.Environment.Parser (ParseEnvError(..), parseEnvIO)

import TldrClient.Locale (Locale, decodeLocale, languagePreference)


data Configuration = Configuration
    { programName :: String
    -- ^ Program name as it should be used in error, debug, and other messages.
    , verbosity :: Verbosity
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
    -- ^ List of sources of pages. See `Source` for details.
    , prefixes :: [Text]
    -- ^ Restrict operations to specific command prefixes. If empty, then there
    -- is no restriction.
    --
    -- > ${prefix}-${command}
    }
  deriving stock (Eq, Show)

decodeStandaloneConfiguration :: String -> Dhall.Decoder Configuration
decodeStandaloneConfiguration programName = Dhall.record do
    verbosity <- Dhall.field "verbosity" Dhall.auto
    colourOutput <- Dhall.field "colourOutput" Dhall.auto
    cacheDirectory <- Dhall.field "cacheDirectory" Dhall.auto
    locale <- Dhall.field "locale" (Dhall.maybe decodeLocale)
    sources <- Dhall.field "sources" (decodeNonEmpty decodeSource)
    pure Configuration
        { prefixes = []
        , ..
        }

decodeSubcommandConfiguration
    :: Verbosity
    -> ColourOutput
    -> String
    -> Dhall.Decoder Configuration
decodeSubcommandConfiguration verbosity colourOutput programName =
    Dhall.record do
        cacheDirectory <- Dhall.field "cacheDirectory" Dhall.auto
        locale <- Dhall.field "locale" (Dhall.maybe decodeLocale)
        sources <- Dhall.field "sources" (decodeNonEmpty decodeSource)
        prefixes <- Dhall.field "prefixes" (decodeNonEmpty Dhall.auto)
        pure Configuration
            { prefixes = NonEmpty.toList prefixes
            , ..
            }

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

getLocales :: Configuration -> Maybe Locale -> IO (NonEmpty Locale)
getLocales config@Configuration{locale} override =
    maybe (parseEnvIO () dieParseEnvError languagePreference) (pure . pure)
        (override <|> locale)
  where
    dieParseEnvError :: ParseEnvError -> IO a
    dieParseEnvError err = do
        putErrorLn config stderr case err of
            ParseEnvError var msg ->
                Text.unpack var
                <> ": Failed to parse environment variable: "
                <> msg
            MissingEnvVarError var ->
                Text.unpack var
                <> ": Missing required environment variable."
            ErrorMessage msg ->
                msg
            UnknownError ->
                "Encountered unknown error, this usually indicates a bug."
        exitFailure

mkDefConfiguration
    :: Maybe Source
    -> Verbosity
    -> ColourOutput
    -> String
    -> IO Configuration
mkDefConfiguration possiblySource verbosity colourOutput programName =
    pure Configuration
        { programName
        , verbosity
        , colourOutput
        , cacheDirectory = Nothing -- Use default path, see `Configuration`.
        , locale = Nothing -- Use locale environment variables, see `getLocales`.
        , sources = pure (fromMaybe tldrPagesOfficialSource possiblySource)
        , prefixes = []
        }
  where
    tldrPagesOfficialSource :: Source
    tldrPagesOfficialSource = Source
        { name = "tldr-pages"
        , format = TldrPagesWithIndex
        , location = Remote (assetsUrl1 :| [assetsUrl2])
        }

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

getUseColours :: Configuration -> Handle -> IO Bool
getUseColours Configuration{colourOutput} handle =
    shouldUseColours handle colourOutput

shouldUseColours :: Handle -> ColourOutput -> IO Bool
shouldUseColours handle = useColoursWhen do
    otputIsTerminal <- hIsTerminalDevice handle
    if otputIsTerminal
        then terminalSupportsColours <$> setupTermFromEnv
        else pure False

putDebugLn :: Configuration -> Handle -> String -> IO ()
putDebugLn Configuration{programName, verbosity} handle msg =
    when (verbosity >= Verbosity.Annoying) do
        hPutStrLn handle (programName <> ": Debug: " <> msg)

putWarningLn :: Configuration -> Handle -> String -> IO ()
putWarningLn Configuration{programName, verbosity} handle msg =
    when (verbosity >= Verbosity.Normal) do
        hPutStrLn handle (programName <> ": Warning: " <> msg)

putErrorLn :: Configuration -> Handle -> String -> IO ()
putErrorLn Configuration{programName, verbosity} handle msg =
    when (verbosity >= Verbosity.Normal) do
        hPutStrLn handle (programName <> ": Error: " <> msg)
