-- |
-- Module:      TldrClient.Configuration
-- Description: Configuration data type and Dhall decoding for it
-- Copyright:   (c) 2021-2024 Peter Trško
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
import Data.Bool (Bool(False, True))
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty (toList)
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
    )
import Text.Show (Show)

import Data.Output.Colour
    ( ColourOutput
    , terminalSupportsColours
    , useColoursWhen
    )
import Data.Text (Text)
import Data.Text qualified as Text (unpack)
import Data.Verbosity (Verbosity)
import Data.Verbosity qualified as Verbosity (Verbosity(Annoying, Normal))
import Dhall (FromDhall)
import Dhall qualified
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

import Data.List.Compat (List, )
import TldrClient.Locale (Locale, )
import TldrClient.Locale qualified as Locale (decode, languagePreference, )


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
    , prefixes :: List Text
    -- ^ Restrict operations to specific command prefixes. If empty, then there
    -- is no restriction.
    --
    -- > ${prefix}-${command}
    , isStandalone :: Bool
    -- ^ Is the tldr client a standalone application or a Command Wrapper
    -- subcommand?
    --
    -- * `True` — the tldr client is a standalone application and not a Command
    --   Wrapper subcommand
    --
    -- * `False` — the tldr client is a Command Wrapper subcommand and not a
    --   standalone application
    }
  deriving stock (Eq, Show)

decodeStandaloneConfiguration :: String -> Dhall.Decoder Configuration
decodeStandaloneConfiguration programName = Dhall.record do
    verbosity <- Dhall.field "verbosity" Dhall.auto
    colourOutput <- Dhall.field "colourOutput" Dhall.auto
    cacheDirectory <- Dhall.field "cacheDirectory" Dhall.auto
    locale <- Dhall.field "locale" (Dhall.maybe Locale.decode)
    sources <- Dhall.field "sources" (decodeNonEmpty decodeSource)
    pure Configuration
        { prefixes = []
        , isStandalone = True
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
        locale <- Dhall.field "locale" (Dhall.maybe Locale.decode)
        sources <- Dhall.field "sources" (decodeNonEmpty decodeSource)
        prefixes <- Dhall.field "prefixes" (decodeNonEmpty Dhall.auto)
        pure Configuration
            { prefixes = NonEmpty.toList prefixes
            , isStandalone = False
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

getLocales :: Configuration -> Handle -> Maybe Locale -> IO (NonEmpty Locale)
getLocales config@Configuration{locale} errorOutput override = maybe
    (parseEnvIO () dieParseEnvError Locale.languagePreference) (pure . pure)
    (override <|> locale)
  where
    dieParseEnvError :: ParseEnvError -> IO a
    dieParseEnvError err = do
        putErrorLn config errorOutput case err of
            ParseEnvError var msg ->
                Text.unpack var
                <> ": Failed to parse environment variable: "
                <> msg
            MissingEnvVarError var ->
                Text.unpack var
                <> ": Missing required environment variable."
            EmptyEnvVarError var ->
                Text.unpack var
                <> ": Required environment variable has an empty value."
            ErrorMessage msg ->
                msg
            UnknownError ->
                "Encountered unknown error, this usually indicates a bug."
        exitFailure

mkDefConfiguration
    :: Bool
    -> Maybe Source
    -> Verbosity
    -> ColourOutput
    -> String
    -> IO Configuration
mkDefConfiguration isStandalone possiblySource verbosity colourOutput
  programName =
    pure Configuration
        { programName
        , verbosity
        , colourOutput
        , cacheDirectory = Nothing -- Use default path, see `Configuration`.
        , locale = Nothing -- Use locale environment variables, see `getLocales`.
        , sources = pure (fromMaybe tldrPagesOfficialSource possiblySource)
        , prefixes = []
        , isStandalone
        }
  where
    tldrPagesOfficialSource :: Source
    tldrPagesOfficialSource = Source
        { name = "tldr-pages"
        , format = TldrPagesWithIndex
        , location = Remote (assetsUrl1 :| [])
        }

    -- URLs are taken from:
    -- <https://github.com/tldr-pages/tldr/blob/v2.2/CLIENT-SPECIFICATION.md>
    assetsUrl1 =
        "https://github.com/tldr-pages/tldr/releases/latest/download/tldr.zip"

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
