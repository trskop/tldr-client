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

import Prelude (fromIntegral)

import Control.Applicative ((<**>), (<*>), (<|>), many, optional, pure, some)
import Control.Exception (throwIO)
import Control.Monad ((>>=), guard)
import Control.Monad.Fail (fail)
import Data.Bool (Bool(False), not, otherwise)
import Data.Eq ((==))
import Data.Foldable (any, for_, null, traverse_)
import Data.Function (($), (.))
import Data.Functor ((<$), (<$>), (<&>))
import qualified Data.List as List (concat, elem, filter, sort, take)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Version (makeVersion)
import Data.Word (Word)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.IO (FilePath, IO)

import Data.CaseInsensitive (CI)
import qualified Data.Either.Validation as Validation
    ( Validation(Failure, Success)
    )
import Data.Text (Text)
import qualified Data.Text as Text
    ( drop
    , intercalate
    , isPrefixOf
    , length
    , uncons
    , unlines
    )
import qualified Data.Text.IO as Text (putStr, putStrLn)
import qualified Dhall (Decoder(Decoder, expected), input, inputFile)
import qualified Options.Applicative as Options
    ( InfoMod
    , Parser
    , ReadM
    , auto
    , eitherReader
    , execParser
    , flag'
    , footer
    , fullDesc
    , header
    , help
    , helper
    , info
    , internal
    , long
    , metavar
    , option
    , short
    , str
    , strArgument
    , strOption
    )
import qualified Prettyprinter (pretty, line)
import qualified Prettyprinter.Render.Terminal as Prettyprinter (putDoc)
import Safe (atDef, initMay, lastDef, lastMay)
import qualified Database.SQLite.Simple as SQLite (withConnection)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , getXdgDirectory
    )

import Configuration
    ( Configuration(Configuration, sources)
    , Source(Source, name)
    , decodeConfiguration
    , getCacheDirectory
    , mkDefConfiguration
    )
import Client
    ( Action(ClearCache, List, Render, Update)
    , SomePlatform
    , client
    , parsePlatform
    )
import Version (VersionInfo(..), prettyVersionInfo)
import Locale (Locale, parseLocale)
import qualified Index (getCommands, getIndexFile, getLocales, getPlatforms)

import Paths_tldr_client (version)


main :: IO ()
main = do
    (config, action) <- parseOptions decodeConfiguration mkDefConfiguration
        completer
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
    | CompletionInfo
    -- ^ Describe how command-line completion works in the form of a Dhall
    -- expression.
    | Completion (Maybe Text) Shell (Maybe Word) [Text]
    -- ^ Do command-line completion.

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
    -> (config -> Shell -> Maybe Word -> [Text] -> IO ())
    -- ^ Command-line completer.
    -> IO (config, Action)
parseOptions decoder@Dhall.Decoder{expected} mkDef completer' = do
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

        CompletionInfo -> do
            Text.putStrLn $ Text.unlines
                [ "let toWordOptions ="
                , "      λ(words : List Text) →"
                , "        List/fold"
                , "          Text"
                , "          words"
                , "          (List Text)"
                , "          (λ(w : Text) → λ(ws : List Text) →\
                                \ [ \"--word=${w}\" ] # ws)"
                , "          ([] : List Text)"
                , ""
                , "in  λ(shell : < Bash | Fish | Zsh >) →"
                , "    λ(index : Natural) →"
                , "    λ(words : List Text) →"
                , "        [ \"--completion\""
                , "        , \"--index=${Natural/show index}\""
                , "        , \"--shell=${merge { Bash = \"bash\",\
                                \ Fish = \"fish\", Zsh = \"zsh\" } shell}\""
                , "        ]"
                , "      # toWordOptions words"
                ]
            exitSuccess

        Completion possiblyConfig shell index words -> do
            config <- parseConfig configFile possiblyConfig
            completer' config shell index words
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
        <|> completionInfoFlag
        <|> (   ( typecheckFlag
                <|> ( completionFlag
                    <*> shellOption
                    <*> optional indexOption
                    <*> many wordOption
                    )
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
      where
        listAction lang platform sources cfg =
            Execute cfg (List platform lang sources)

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
      where
        clearCacheAction lang platform sources cfg =
            Execute cfg (ClearCache platform lang sources)

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
      where
        updateAction sources cfg = Execute cfg (Update sources)

    sourceOption :: Options.Parser Text
    sourceOption = Options.strOption
        ( Options.long "source"
        <> Options.short 's'
        <> Options.metavar "SOURCE"
        <> Options.help "Show, list, or update cache only for specified\
            \ SOURCEs; by default all sources are used; this option can be\
            \ used multiple times to specify multiple SOURCEs"
        )

    completionInfoFlag
        :: Options.Parser Mode
    completionInfoFlag = Options.flag' CompletionInfo
        ( Options.long "completion-info"
        <> Options.help "Describe how command-line completion works in the\
            \ form of a  Dhall experssion"
        <> Options.internal
        )

    completionFlag
        :: Options.Parser (Shell -> Maybe Word -> [Text] -> Maybe Text -> Mode)
    completionFlag = Options.flag' completionMode
        ( Options.long "completion"
        <> Options.help "Provide command-line completion"
        <> Options.internal
        )
      where
        completionMode shell index words config =
            Completion config shell index words

    indexOption :: Options.Parser Word
    indexOption = Options.option Options.auto
        ( Options.long "index"
        <> Options.help "Index of a WORD that we want completion for"
        <> Options.metavar "INDEX"
        <> Options.internal
        )

    shellOption :: Options.Parser Shell
    shellOption = Options.option parseShell
        ( Options.long "shell"
        <> Options.help "Shell for which we want completion for"
        <> Options.metavar "SHELL"
        <> Options.internal
        )
      where
        parseShell :: Options.ReadM Shell
        parseShell = Options.str @(CI String) >>= \case
            "bash" ->
                pure Bash
            "fish" ->
                pure Fish
            "zsh" ->
                pure Zsh
            _ ->
                fail "Unrecognised shell name, expected 'bash', 'fish', or\
                    \ 'zsh'"

    wordOption :: Options.Parser Text
    wordOption = Options.strOption
        ( Options.long "word"
        <> Options.metavar "WORD"
        <> Options.internal
        )

data Shell = Bash | Fish | Zsh

completer :: Configuration -> Shell -> Maybe Word -> [Text] -> IO ()
completer config@Configuration{sources} _shell index words
  | previousOneOf ["--platform", "-p"] =
        completePlatform "" current

  | previousOneOf ["--language", "-L"] =
        completeLanguage "" current

  | previousOneOf ["--source", "-s"] =
        completeSource "" current

  | previous == Just "--config" =
        completeConfig "" current

  | Just ('-', _) <- Text.uncons current = if
      | "--platform=" `Text.isPrefixOf` current ->
            completePlatform "--platform=" current

      | "--language=" `Text.isPrefixOf` current ->
            completeLanguage "--language=" current

      | "--source=" `Text.isPrefixOf` current ->
            completeSource "--source=" current

      | "--config=" `Text.isPrefixOf` current ->
            completeConfig "--config=" current

        -- Value of `current` is the first option on the command-line.
      | null before -> do
            traverse_ Text.putStrLn (prefixMatch current topLevelOptions)

        -- These options mean that nothing else should be completed.
      | hadBeforeOneOf topLevelTerminalOptions ->
            pure ()

      | hadBefore "--config-typecheck" -> do
            let possibilities :: [Text]
                possibilities = do
                    let opt = "--config="
                    opt <$ guard (not (hadBefore opt))
            traverse_ Text.putStrLn (prefixMatch current possibilities)

      | hadBeforeOneOf ["--update", "-u"] -> do
            let possibilities :: [Text]
                possibilities = List.concat
                    [ ["--source=", "-s"]
                    , do
                        guard (not (hadBeforePrefix "--config="))
                        guard (not (hadBefore "--config"))
                        pure "--config="
                    ]
            traverse_ Text.putStrLn (prefixMatch current possibilities)

        -- "--list", "-l", "--clear-cache", or default mode:
      | otherwise -> do
            let possibilities :: [Text]
                possibilities = List.concat
                    [ ["--source=", "-s"]
                    , do
                        guard (not (hadBeforePrefix "--config="))
                        guard (not (hadBefore "--config"))
                        pure "--config="
                    , do
                        guard (not (hadBeforePrefix "--language=")
                        guard (not (hadBeforeOneOf ["--language", "-L"]))
                        ["--language=", "-L"]
                    , do
                        guard (not (hadBeforePrefix "--platform="))
                        guard (not (hadBeforeOneOf ["--platform", "-p"]))
                        ["--platform=", "-p"]
                    ]
            traverse_ Text.putStrLn (prefixMatch current possibilities)

  | hadBeforeOneOf notDefaultModeOptions =
      -- There are not arguments, only options in these modes.
      pure ()

  | otherwise =
        completeArgument
            ( List.filter (not . ("-" `Text.isPrefixOf`)) before
            <> [current]
            )
  where
    before :: [Text]
    before
      | null words = []
      | otherwise  = maybe [] (\i -> List.take (fromIntegral i) words) index

    previous :: Maybe Text
    previous = lastMay before

    current :: Text
    current = maybe (lastDef "" words) (atDef "" words . fromIntegral) index

    previousOneOf :: [Text] -> Bool
    previousOneOf opts = maybe False (`List.elem` opts) previous

    hadBefore :: Text -> Bool
    hadBefore opt = opt `List.elem` before

    hadBeforeOneOf :: [Text] -> Bool
    hadBeforeOneOf opts = any (`List.elem` opts) before

    hadBeforePrefix :: Text -> Bool
    hadBeforePrefix prefix = any (prefix `Text.isPrefixOf`) before

    topLevelOptions :: [Text]
    topLevelOptions =
        [ "--help", "-h"
        , "--version", "-v"
        , "--config-print-type"
        , "--config-typecheck"
        , "--update", "-u"
        , "--list", "-l"
        , "--clear-cache"
        , "--language=", "-L"
        , "--platform=", "-p"
        , "--source=", "-s"
        , "--config="
        ]

    topLevelTerminalOptions :: [Text]
    topLevelTerminalOptions =
        [ "--help", "-h"
        , "--version", "-v"
        , "--config-print-type"
        ]

    notDefaultModeOptions :: [Text]
    notDefaultModeOptions =
        [ "--help", "-h"
        , "--version", "-v"
        , "--config-print-type"
        , "--config-typecheck"
        , "--update", "-u"
        , "--list", "-l"
        , "--clear-cache"
        ]

    prefixMatch :: Text -> [Text] -> [Text]
    prefixMatch prefix options =
        List.sort (List.filter (prefix `Text.isPrefixOf`) options)

    completeLanguage :: Text -> Text -> IO ()
    completeLanguage = withPrefix \word -> do
        cacheDir <- getCacheDirectory config
        Index.getIndexFile cacheDir >>= \case
            Nothing ->
                pure []
            Just indexFile ->
                SQLite.withConnection indexFile \connection ->
                    List.sort <$> Index.getLocales connection word

    completePlatform :: Text -> Text -> IO ()
    completePlatform = withPrefix \word -> do
        let extra = prefixMatch word ["all"]
        cacheDir <- getCacheDirectory config
        list <- Index.getIndexFile cacheDir >>= \case
            Nothing ->
                pure []
            Just indexFile ->
                SQLite.withConnection indexFile \connection ->
                    Index.getPlatforms connection word
        pure (List.sort (extra <> list))

    completeSource :: Text -> Text -> IO ()
    completeSource = withPrefix \word ->
        pure . prefixMatch word $ NonEmpty.toList sources <&> \Source{name} ->
            name

    -- TODO: We should support file completion when it starts with one of the
    -- following characters: '~', '.', '/'. We also have to preserve '~'
    -- character.
    completeConfig :: Text -> Text -> IO ()
    completeConfig = withPrefix \_ -> pure []

    completeArgument :: [Text] -> IO ()
    completeArgument cmds = do
        cacheDir <- getCacheDirectory config
        Index.getIndexFile cacheDir >>= \case
            Nothing ->
                pure ()
            Just indexFile -> do
                list <- SQLite.withConnection indexFile \connection -> do
                    let cmd = Text.intercalate "-" cmds
                    List.sort <$> Index.getCommands connection cmd
                for_ list \completion -> do
                    let prefix = Text.intercalate "-"
                            (maybe [] (<> [""]) (initMay cmds))
                    Text.putStrLn (Text.drop (Text.length prefix) completion)

    withPrefix :: (Text -> IO [Text]) -> Text -> Text -> IO ()
    withPrefix f prefix word = do
        completions <- f (Text.drop (Text.length prefix) word)
        for_ completions \completion -> do
            Text.putStr prefix
            Text.putStrLn completion
