-- |
-- Module:      Main
-- Description: Client for tldr-pages
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Client for [tldr-pages](https://tldr.sh/).
module Main
    ( main
    )
  where

import Control.Applicative ((<**>), (<*>), (<|>), many, optional, some)
import Control.Exception (throwIO)
import Control.Monad ((>>=))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Version (makeVersion)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.IO (FilePath, IO)

import qualified Data.Either.Validation as Validation
  ( Validation(Failure, Success)
  )
import Data.Text (Text)
import qualified Dhall (Decoder(Decoder, expected), input, inputFile)
import qualified Options.Applicative as Options
    ( InfoMod
    , Parser
    , eitherReader
    , execParser
    , flag'
    , footer
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , short
    , strArgument
    , strOption
    )
import qualified Prettyprinter (pretty, line)
import qualified Prettyprinter.Render.Terminal as Prettyprinter (putDoc)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , getXdgDirectory
    )

import Configuration (decodeConfiguration, mkDefConfiguration)
import Client
    ( Action(ClearCache, List, Render, Update)
    , SomePlatform
    , client
    , parsePlatform
    )
import Version (VersionInfo(..), prettyVersionInfo)
import Locale (Locale, parseLocale)

import Paths_tldr_client (version)


main :: IO ()
main = do
    (config, action) <- parseOptions decodeConfiguration mkDefConfiguration
    client config action

data Mode
    = Execute (Maybe Text) Action
    -- ^ We want to execute the application with the given configuration. If
    -- the configuration is 'Nothing' we need to read the environment variable
    -- or configuration file.
    | Typecheck (Maybe Text)
    -- ^ We want to do a dry-run during which we only typecheck the
    -- configuration. If the configuration is 'Nothing' we need to read the
    -- environment variable or configuration file.
    | PrintType
    -- ^ Instead of doing anything just print the type of the configuration the
    -- application expects to be given.
    | Version
    -- ^ Print version information and exit instead of doing anything.
    -- Configuration will not be parsed or used in any way.

-- | Parse command line options, handle everything that is not related to the
-- main purpose of this binary or return `Configuration` otherwise.
--
-- When reading configuration this priority is used:
--
-- 1. Command-line option: `--config=EXPR`
-- 2. Environment variable: `TLDR_CONFIG=EXPR`
-- 3. Config file: `${XDG_CONFIG_HOME:-${HOME}/.config}/tldr/config.dhall`
-- 4. Default configuration
--
-- Notes:
--
-- * Keep the configuration type polymorphic (@config@ type variable) instead
--   of making it monomorphic. This will allow us to enforce that this code
--   only deals with command-line interface.
parseOptions
    :: forall config
    .  Dhall.Decoder config
    -- ^ Dhall 'Dhall.Decoder' consists of parser and expected type. Dhall
    -- library provides one special 'Dhall.Decoder':
    --
    -- @
    -- 'Dhall.auto' :: 'Dhall.FromDhall' a => 'Dhall.Decoder' a
    -- @
    --
    -- Which allows us to use type class mechanism for deriving and combining
    -- parsers and is a good default in many cases.
    -> IO config
    -- ^ Construct default configuration if there is no configuration
    -- available.
    -> IO (config, Action)
parseOptions decoder@Dhall.Decoder{expected} mkDef = do
    configFile <- getXdgDirectory XdgConfig "tldr/config.dhall"

    parseOptions' configFile >>= \case
        Execute possiblyConfig action ->
            (, action) <$> parseConfig configFile possiblyConfig

        Typecheck config -> do
            -- TODO: When there is no config file (mkDef used) then we should
            -- probably complain.
            _ <- parseConfig configFile config
            exitSuccess

        PrintType -> do
            printType
            exitSuccess

        Version -> do
            Prettyprinter.putDoc $ prettyVersionInfo VersionInfo
                { clientVersion = version
                , tldrClientSpecificationVersion = makeVersion [1,5]
                }
            exitSuccess
  where
    parseConfig :: FilePath -> Maybe Text -> IO config
    parseConfig configFile possiblyExpr = do
        case possiblyExpr of
            Just expr ->
                Dhall.input decoder expr

            Nothing ->
                lookupEnv "TLDR_CONFIG" >>= \case
                    Just expr ->
                        Dhall.input decoder (fromString expr)

                    Nothing -> do
                        configExists <- doesFileExist configFile
                        if configExists
                            then
                                Dhall.inputFile decoder configFile
                            else
                                mkDef

    printType :: IO ()
    printType = case expected of
        Validation.Success expr ->
            Prettyprinter.putDoc
                ( Prettyprinter.pretty expr
                <> Prettyprinter.line
                )

        Validation.Failure err ->
            -- This indicates a bug in the Decoder.
            throwIO err

    infoMod :: FilePath -> Options.InfoMod a
    infoMod configFile = Options.fullDesc
        <> Options.header "Client for tldr-pages."
        <> Options.footer (footer configFile)

    footer :: FilePath -> String
    footer = ("User configuration file is read from: " <>)

    parseOptions' :: FilePath -> IO Mode
    parseOptions' configFile = Options.execParser
        (Options.info (options <**> Options.helper) (infoMod configFile))

    options :: Options.Parser Mode
    options =
        versionFlag
        <|> printTypeFlag
        <|> (   ( typecheckFlag
                <|> (   ( updateFlag
                        <|> (   ( listFlag
                                <|> clearCacheFlag
                                <|> (renderAction <$> some commandArgument)
                                )
                            <*> optional languageOption
                            <*> optional platformOption
                            )
                        )
                    <*> many sourceOption
                    )
                )
            <*> optional configOption
            )

    renderAction
        :: [String]
        -> Maybe Locale
        -> Maybe SomePlatform
        -> [Text]
        -> Maybe Text
        -> Mode
    renderAction cmds lang platform sources cfg =
        Execute cfg (Render platform lang sources cmds)

    listAction
        :: Maybe Locale
        -> Maybe SomePlatform
        -> [Text]
        -> Maybe Text
        -> Mode
    listAction lang platform sources cfg =
        Execute cfg (List platform lang sources)

    clearCacheAction
        :: Maybe Locale
        -> Maybe SomePlatform
        -> [Text]
        -> Maybe Text
        -> Mode
    clearCacheAction lang platform sources cfg =
        Execute cfg (ClearCache platform lang sources)

    updateAction
        :: [Text]
        -> Maybe Text
        -> Mode
    updateAction sources cfg = Execute cfg (Update sources)

    configOption :: Options.Parser Text
    configOption = Options.strOption
        ( Options.long "config"
        <> Options.metavar "EXPR"
        <> Options.help "Set configuration to EXPR, where EXPR is a Dhall\
            \ expression; if application fails to parse or typecheck the EXPR\
            \ it terminates with exit code 1"
        )

    typecheckFlag :: Options.Parser (Maybe Text -> Mode)
    typecheckFlag = Options.flag' Typecheck
        ( Options.long "config-typecheck"
        <> Options.help "Typecheck the configuration and exit; exit code 0 is\
            \ used on success and exit code 1 on failure to typecheck"
        )

    printTypeFlag :: Options.Parser Mode
    printTypeFlag = Options.flag' PrintType
        ( Options.long "config-print-type"
        <> Options.help "Print Dhall type of configuration accepted by the\
            \ application"
        )

    versionFlag :: Options.Parser Mode
    versionFlag = Options.flag' Version
        ( Options.long "version"
        <> Options.short 'v' -- Mandated by Tldr Client Specification
        <> Options.help "Print version information to standard output and\
            \ terminate with exit code 0"
        )

    listFlag :: Options.Parser
        (  Maybe Locale
        -> Maybe SomePlatform
        -> [Text]
        -> Maybe Text
        -> Mode
        )
    listFlag = Options.flag' listAction
        ( Options.long "list"
        <> Options.short 'l'
        <> Options.help "Lists all the pages for the current platform to the\
            \ standard output; if '--platform=all' is specified then all pages\
            \ in all platforms are listed"
        )

    clearCacheFlag :: Options.Parser
        (  Maybe Locale
        -> Maybe SomePlatform
        -> [Text]
        -> Maybe Text
        -> Mode
        )
    clearCacheFlag = Options.flag' clearCacheAction
        ( Options.long "clear-cache"
        <> Options.help "Clear offline cache; by default the whole cache is\
            \ purged, but if '--source=SOURCE', '--platform=PLATFORM', or\
            \ '--language=LANGUAGE' are specified then they limit what parts\
            \ of the cache are removed."
        )

    platformOption :: Options.Parser SomePlatform
    platformOption = Options.option (Options.eitherReader parsePlatform)
        ( Options.long "platform"
        <> Options.short 'p'
        <> Options.metavar "PLATFORM"
        <> Options.help "Search or list pages for specified PLATFORM. If not\
            \ option is omitted then the platform the application is running\
            \ on is used as a default"
        )

    languageOption :: Options.Parser Locale
    languageOption = Options.option
        (Options.eitherReader (parseLocale . fromString))
        ( Options.long "language"
        <> Options.short 'L'
        <> Options.metavar "LANGUAGE"
        <> Options.help "Search/list pages written in LANGUAGE. Overrides\
            \ default language detection mechanism"
        )

    commandArgument :: Options.Parser String
    commandArgument = Options.strArgument
        ( Options.metavar "COMMAND"
        <> Options.help "Show pages for COMMAND. Multiple COMMANDs can be\
            \ specified, in which case they are treated as one command with\
            \ dashes in between. For example, \"tldr git commit\" is the same\
            \ as \"tldr git-commit\""
        )

    updateFlag :: Options.Parser ([Text] -> Maybe Text -> Mode)
    updateFlag = Options.flag' updateAction
        ( Options.long "update"
        <> Options.short 'u'
        <> Options.help "Updates the offline cache of pages; if\
            \ '--sources=SOURCE' is specified only cache for those page\
            \ SOURCEs is updated"
        )

    sourceOption :: Options.Parser Text
    sourceOption = Options.strOption
        ( Options.long "source"
        <> Options.short 's'
        <> Options.metavar "SOURCE"
        <> Options.help "Show, list, or update cache only for specified\
            \ SOURCEs; by default all sources are used; this option can be\
            \ used multiple times to specify multiple SOURCEs"
        )
