-- |
-- Module:      TldrClient.Options
-- Description: Command-line options for a Tldr Client binary
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Command-line options for a Tldr Client binary.
module TldrClient.Options
    ( ProgramName(..)
    , Params(..)
    , parse
    , completer
    )
  where

import Prelude ((+), fromIntegral)

import Control.Applicative ((<**>), (<*>), (<|>), many, optional, pure, some)
import Control.Exception (throwIO)
import Control.Monad ((>>=), guard)
import Control.Monad.Fail (fail)
import Data.Bool (Bool(False), not, otherwise)
import Data.Char (Char)
import Data.Eq ((==))
import Data.Foldable (any, concat, for_, length, null, sum)
import Data.Function (($), (.))
import Data.Functor (($>), (<$), (<$>), (<&>), fmap)
import Data.Int (Int)
import qualified Data.List as List
    ( concat
    , elem
    , filter
    , repeat
    , sort
    , take
    , zipWith
    )
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import qualified Data.String as String (words)
import Data.Traversable (for)
import Data.Version (Version, makeVersion, showVersion)
import Data.Word (Word)
import System.Environment (getArgs, getEnvironment)
import System.Exit
    ( ExitCode(ExitSuccess)
    , exitFailure
    , exitSuccess
    , exitWith
    )
import System.IO
    ( FilePath
    , Handle
    , IO
    , hIsTerminalDevice
    , hPutStrLn
    , stderr
    , stdout
    )

import Data.CaseInsensitive (CI)
import qualified Data.Either.Validation as Validation
    ( Validation(Failure, Success)
    )
import Data.Output.Colour (ColourOutput)
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
import Data.Verbosity (Verbosity)
import qualified Dhall (Decoder(expected), input, inputFile)
import qualified Options.Applicative as Options
    ( InfoMod
    , Parser
    , ParserHelp(ParserHelp, helpBody, helpFooter, helpUsage)
    , ParserResult(CompletionInvoked, Failure, Success)
    , ReadM
    , auto
    , defaultPrefs
    , eitherReader
    , execFailure
    , execFailure
    , execParserPure
    , flag'
    , footerDoc
    , fullDesc
    , header
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
import Options.Applicative.Help ((<+>))
import qualified Options.Applicative.Help as Options
    ( Doc
    , bold
    , braces
    , brackets
    , dullgreen
    , encloseSep
    , fillSep
    , hang
    , magenta
    , nest
    , renderHelp
    , squotes
    , string
    , underline
    , vsep
    )
import qualified Prettyprinter (pretty, line)
import qualified Prettyprinter.Render.Terminal as Prettyprinter (putDoc)
import Safe (atDef, initMay, lastDef, lastMay)
import qualified Database.SQLite.Simple as SQLite (withConnection)
import System.Console.Terminal.Size as Terminal (Window(Window, width), hSize)
import System.Directory (doesFileExist)

import TldrClient.Client
    ( Action(ClearCache, List, Render, Update)
    , SomePlatform
    , parsePlatform
    )
import TldrClient.Configuration
    ( Configuration(Configuration, prefixes, sources)
    , Source(Source, name)
    , getCacheDirectory
    , shouldUseColours
    )
import qualified TldrClient.Index as Index
    ( getCommands
    , getIndexFile
    , getLocales
    , getPlatforms
    )
import TldrClient.Locale (Locale, parseLocale)
import TldrClient.Version (VersionInfo(..), prettyVersionInfo)


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
    | PrintVersion
    -- ^ Print version information and exit instead of doing anything.
    -- Configuration will not be parsed or used in any way.
    | PrintCompletionInfo
    -- ^ Describe how command-line completion works in the form of a Dhall
    -- expression.
    | Complete (Maybe Text) Shell (Maybe Word) [Text]
    -- ^ Do command-line completion.

data ProgramName
    = StandaloneApplication String
    -- ^ Executable \/ command name.
    | CommandWrapperSubcommand String String
    -- ^ Toolset and subcommand name.

programNameToString :: ProgramName -> String
programNameToString = \case
    StandaloneApplication command ->
        command
    CommandWrapperSubcommand toolset subcommand ->
        toolset <> " " <> subcommand

-- | Generate program name as used in usage lines.
--
-- In case of `StandaloneApplication` it's only:
--
-- > ${command}
--
-- And in case of `CommandWrapperSubcommand` it is:
--
-- > ${toolset} [GLOBAL_OPTIONS] ${subcommand}
programNameToDoc :: PrettyUtils -> ProgramName -> Options.Doc
programNameToDoc PrettyUtils{metavar} = \case
    StandaloneApplication command ->
        Options.string command
    CommandWrapperSubcommand toolset subcommand ->
        Options.fillSep
            [ Options.string toolset
            , Options.brackets (metavar "GLOBAL_OPTIONS")
            , Options.string subcommand
            ]

-- | Generates usage line like:
--
-- > ${toolset} [GLOBAL_OPTIONS] help [--man] ${subcommand}
--
-- Returns `Nothing` when `ProgramName` is `StandaloneApplication` as the above
-- usage line is useful only for Command Wrapper subcommands.
programNameToHelpDoc :: PrettyUtils -> ProgramName -> Maybe Options.Doc
programNameToHelpDoc PrettyUtils{flag, metavar} = \case
    StandaloneApplication _ ->
        Nothing
    CommandWrapperSubcommand toolset subcommand ->
        Just $ Options.fillSep
            [ Options.string toolset
            , Options.brackets (metavar "GLOBAL_OPTIONS")
            , "help"
            , Options.brackets (flag "man")
            , Options.string subcommand
            ]

-- | Parameters passed to `parse` function to avoid ad-hoc function arguments
-- antipattern.
data Params config = Params
    { version :: Version
    -- ^ Application version printed in @--version@ mode.
    , colourOutput :: ColourOutput
    , verbosity :: Verbosity
    , programName :: ProgramName
    -- ^ Command \/ application name as it is visible to the user.
    , configFile :: FilePath
    -- ^ Configuration file to read when there is no configuration provided.
    , configurationExpression :: Maybe Text
    -- ^ Configuration expression passed via environment variable. `Nothing` if
    -- no such environment variable was set.
    , decoder :: String -> Dhall.Decoder config
    -- ^ Dhall 'Dhall.Decoder' consists of parser and expected type. The
    -- `String` parameter is rendered @programName :: `ProgramName`@ field
    -- value.
    , mkDefault :: Verbosity -> ColourOutput -> String -> IO config
    -- ^ Construct default configuration if there is no configuration
    -- available. The `String` parameter is rendered
    -- @programName :: `ProgramName`@ field value.
    , runCompletion :: config -> Shell -> Maybe Word -> [Text] -> IO ()
    -- ^ Command-line completer.
    }

-- | Parse command line options, handle everything that is not related to the
-- main purpose of this binary or return `Configuration` otherwise.
--
-- When reading configuration this priority is used:
--
-- 1. Command-line option: `--config=EXPR`.
-- 2. Environment variable value (Dhall expression) from
--    @configurationExpression@, see `Params`.
-- 3. Configuration file @configFile@, see `Params`.
-- 4. Default configuration generated by @mkDefault@, see `Params`.
--
-- Notes:
--
-- * Keep the configuration type polymorphic (@config@ type variable) instead
--   of making it monomorphic. This will allow us to enforce that this code
--   only deals with command-line interface.
parse :: forall config. Params config -> IO (config, Action)
parse Params{..} = do
    execOptionsParser >>= \case
        Execute possiblyConfig action ->
            (, action) <$> parseConfig possiblyConfig

        Typecheck config -> do
            -- TODO: When there is no config file (mkDefault used) then we
            -- should probably complain.
            _ <- parseConfig config
            exitSuccess

        PrintType -> do
            printType
            exitSuccess

        PrintVersion -> do
            Prettyprinter.putDoc $ prettyVersionInfo VersionInfo
                { clientVersion = version
                , tldrClientSpecificationVersion = makeVersion [1,5]
                }
            exitSuccess

        PrintCompletionInfo -> do
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

        Complete possiblyConfig shell index words -> do
            config <- parseConfig possiblyConfig
            runCompletion config shell index words
            exitSuccess
  where
    parseConfig :: Maybe Text -> IO config
    parseConfig commandLineExpr = do
        let decoder' = decoder programNameStr
        case commandLineExpr of
            Just expr ->
                -- Configuration passed via command line option: `--config=EXPR`
                Dhall.input decoder' expr

            Nothing ->
                case configurationExpression of
                    Just expr ->
                        -- Configuration passed via environment variable.
                        Dhall.input decoder' expr

                    Nothing -> do
                        configExists <- doesFileExist configFile
                        if configExists
                            then
                                Dhall.inputFile decoder' configFile
                            else
                                mkDefault verbosity colourOutput programNameStr

    printType :: IO ()
    printType = case Dhall.expected (decoder programNameStr) of
        Validation.Success expr ->
            Prettyprinter.putDoc
                ( Prettyprinter.pretty expr
                <> Prettyprinter.line
                )

        Validation.Failure err ->
            -- This indicates a bug in the Decoder.
            throwIO err

    programNameStr :: String
    programNameStr = programNameToString programName

    infoMod :: Options.InfoMod a
    infoMod = Options.fullDesc
        <> Options.header "Display and query tldr-pages."
        <> Options.footerDoc (Just $ footerDoc (mkPrettyUtils \_ _ x -> x))

    footerDoc :: PrettyUtils -> Options.Doc
    footerDoc PrettyUtils{value} =
        "User configuration file is read from:"
        <+> value (Options.string configFile)

    execOptionsParser :: IO Mode
    execOptionsParser = do
        let parserInfo = Options.info (options <**> Options.helper) infoMod
        args <- getArgs
        case Options.execParserPure Options.defaultPrefs parserInfo args of
            Options.Success r ->
                pure r

            Options.Failure failure -> do
                let (help, exit, _) =
                        Options.execFailure failure programNameStr

                    Options.ParserHelp{helpBody, helpFooter, helpUsage} = help

                    renderHelp :: PrettyUtils -> Int -> String
                    renderHelp prettyUtils cols = Options.renderHelp cols help
                        -- We want to set better help message iff the original
                        -- help message piece is not empty. This way we are
                        -- respecting options parser preferences.
                        { Options.helpUsage =
                            helpUsage $> usage prettyUtils
                        , Options.helpBody =
                            helpBody $> optionsDoc prettyUtils
                        , Options.helpFooter =
                            helpFooter $> footerDoc prettyUtils
                        }

                    handle :: Handle
                    handle = if exit == ExitSuccess then stdout else stderr

                applyTerminalStyle <- do
                    useColours <- shouldUseColours handle colourOutput
                    isTerminal <- hIsTerminalDevice handle
                    pure \applyColours applyDecoration doc -> if
                      | useColours -> applyColours doc
                      | isTerminal -> applyDecoration doc
                      | otherwise  -> doc
                cols <- Terminal.hSize handle <&> \case
                    Nothing -> 80
                    Just Terminal.Window{width} -> width
                let prettyUtils = mkPrettyUtils applyTerminalStyle
                hPutStrLn handle (renderHelp prettyUtils cols)
                exitWith exit

            Options.CompletionInvoked{} ->
                -- We don't use optparse-applicative command line completion.
                exitFailure

    usage :: PrettyUtils -> Options.Doc
    usage prettyUtils = Options.nest 2 $ Options.vsep
        (   [ "Usage:"
            , ""
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , configDoc, platformDoc, languageDoc, sourceDoc
                , metavar "COMMAND"
                , Options.brackets (metavar "SUBCOMMAND" <+> ellipsis)
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces (alt [flag "list", shortFlag 'l'])
                , configDoc, platformDoc, languageDoc, sourceDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces (alt [flag "update", shortFlag 'u'])
                , configDoc, platformDoc, languageDoc, sourceDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , flag "clear-cache"
                , configDoc, platformDoc, languageDoc, sourceDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces
                    (alt [flag "config-typecheck", flag "config-print-type"])
                , configDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces (alt [flag "version", shortFlag 'v'])
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces (alt [flag "help", shortFlag 'h'])
                ]
            ]
        <> commandWrapperHelp
        )
      where
        PrettyUtils
            { alt
            , ellipsis
            , flag
            , metavar
            , opt
            , shortFlag
            , shortOpt
            } = prettyUtils

        programName' :: Options.Doc
        programName' = programNameToDoc prettyUtils programName

        configDoc :: Options.Doc
        configDoc = Options.brackets (opt "config" "EXPR")

        platformDoc :: Options.Doc
        platformDoc = Options.brackets
            ( Options.braces
                (alt [opt "platform" "PLATFORM", shortOpt 'p' "PLATFORM"])
            <+> ellipsis
            )

        languageDoc :: Options.Doc
        languageDoc = Options.brackets
            ( Options.braces
                (alt [opt "language" "LANGUAGE", shortOpt 'L' "LANGUAGE"])
            <+> ellipsis
            )

        sourceDoc :: Options.Doc
        sourceDoc = Options.brackets
            ( Options.braces
                (alt [opt "source" "SOURCE", shortOpt 's' "SOURCE"])
            <+> ellipsis
            )

        commandWrapperHelp :: [Options.Doc]
        commandWrapperHelp =
            case programNameToHelpDoc prettyUtils programName of
                Nothing -> []
                Just doc -> [Options.hang 2 doc]

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
        )

    typecheckFlag :: Options.Parser (Maybe Text -> Mode)
    typecheckFlag = Options.flag' Typecheck (Options.long "config-typecheck")

    printTypeFlag :: Options.Parser Mode
    printTypeFlag = Options.flag' PrintType (Options.long "config-print-type")

    versionFlag :: Options.Parser Mode
    versionFlag = Options.flag' PrintVersion
        ( Options.long "version"
        <> Options.short 'v' -- Mandated by Tldr Client Specification
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
        )
      where
        clearCacheAction lang platform sources cfg =
            Execute cfg (ClearCache platform lang sources)

    platformOption :: Options.Parser SomePlatform
    platformOption = Options.option (Options.eitherReader parsePlatform)
        ( Options.long "platform"
        <> Options.short 'p'
        <> Options.metavar "PLATFORM"
        )

    languageOption :: Options.Parser Locale
    languageOption = Options.option
        (Options.eitherReader (parseLocale . fromString))
        ( Options.long "language"
        <> Options.short 'L'
        <> Options.metavar "LANGUAGE"
        )

    commandArgument :: Options.Parser String
    commandArgument = Options.strArgument (Options.metavar "COMMAND")

    updateFlag :: Options.Parser ([Text] -> Maybe Text -> Mode)
    updateFlag = Options.flag' updateAction
        ( Options.long "update"
        <> Options.short 'u'
        )
      where
        updateAction sources cfg = Execute cfg (Update sources)

    sourceOption :: Options.Parser Text
    sourceOption = Options.strOption
        ( Options.long "source"
        <> Options.short 's'
        <> Options.metavar "SOURCE"
        )

    completionInfoFlag
        :: Options.Parser Mode
    completionInfoFlag = Options.flag' PrintCompletionInfo
        ( Options.long "completion-info"
        <> Options.internal
        )

    completionFlag
        :: Options.Parser (Shell -> Maybe Word -> [Text] -> Maybe Text -> Mode)
    completionFlag = Options.flag' completionMode
        ( Options.long "completion"
        <> Options.internal
        )
      where
        completionMode shell index words config =
            Complete config shell index words

    indexOption :: Options.Parser Word
    indexOption = Options.option Options.auto
        ( Options.long "index"
        <> Options.metavar "INDEX"
        <> Options.internal
        )

    shellOption :: Options.Parser Shell
    shellOption = Options.option parseShell
        ( Options.long "shell"
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

    -- Format:
    --
    -- > Options:
    -- >
    -- > ⎵⎵ARGUMENT
    -- > ⎵⎵⎵⎵⎵⎵Description
    -- >
    -- > ⎵⎵--long-option, -s
    -- > ⎵⎵⎵⎵⎵⎵Use long option first as it is much more descriptive than short
    -- > ⎵⎵⎵⎵⎵⎵one so that when user is skimming the help message they can find
    -- > ⎵⎵⎵⎵⎵⎵things faster, sometimes even by skipping reading the
    -- > ⎵⎵⎵⎵⎵⎵description.
    -- >
    -- > ⎵⎵--another-long-option=ARG, -a ARG
    -- > ⎵⎵⎵⎵⎵⎵...
    -- >
    -- > ...
    optionsDoc :: PrettyUtils -> Options.Doc
    optionsDoc prettyUtils = Options.nest 2 $ Options.vsep
        (   [ "Options:"
            , ""
            , Options.nest 4 $ Options.vsep
                [ metavar "COMMAND"
                    <+> Options.brackets (metavar "SUBCOMMAND" <+> ellipsis)
                , Options.fillSep
                    [ paragraph "Show pages for a"
                    , metavar "COMMAND" <> "."
                    , "When"
                    , metavar "COMMAND"
                    , paragraph "is followed by"
                    , metavar "SUBCOMMAND" <> "s"
                    , paragraph "then they are treated as one command with\
                        \ dashes in between. For example,"
                    , "\"" <> value "tldr git commit" <> "\""
                    , paragraph "is the same as"
                    , "\"" <> value "tldr git-commit" <> "\"."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ list [opt "platform" "PLATFORM", shortOpt 'p' "PLATFORM"]
                , Options.fillSep
                    [ paragraph "Search or list pages for specified"
                    , metavar "PLATFORM" <> ";"
                    , paragraph "this option can be used multiple times to\
                        \ specify multiple"
                    , metavar "PLATFORM" <> "s."
                    , paragraph "If not option is omitted then the platform\
                        \ the application is running on is used as a default."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ list [opt "language" "LANGUAGE", shortOpt 'L' "LANGUAGE"]
                , Options.fillSep
                    [ paragraph "Search/list pages written in"
                    , metavar "LANGUAGE" <> ";"
                    , paragraph "this option can be used multiple times to\
                        \ specify multiple"
                    , metavar "LANGUAGE" <> "s."
                    , paragraph "Overrides default language detection\
                        \ mechanism."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ list [opt "source" "SOURCE", shortOpt 's' "SOURCE"]
                , Options.fillSep
                    [ paragraph "Show, list, or update cache only for\
                        \ specified"
                    , metavar "SOURCE" <> "s;"
                    , paragraph "by default all sources are used; this option\
                        \ can be used multiple times to specify multiple"
                    , metavar "SOURCE" <> "s."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ list [flag "list", shortFlag 'l']
                , Options.fillSep
                    [ paragraph "Lists all the pages for the current platform\
                      \ to the standard output; if"
                    , Options.squotes (flag "platform" <> "=" <> value "all")
                    , paragraph "is specified then all pages in all platforms\
                        \ are listed."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ list [flag "update", shortFlag 'u']
                , Options.fillSep
                    [ paragraph "Updates the offline cache of pages; if"
                    , Options.squotes (opt "sources" "SOURCE")
                    , paragraph "is specified only cache for those page"
                    , metavar "SOURCE" <> "s"
                    , paragraph "is updated."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ flag "clear-cache"
                , Options.fillSep
                    [ paragraph "Updates the offline cache of pages; if"
                    , Options.squotes (opt "sources" "SOURCE")
                    , paragraph "is specified only cache for those page"
                    , metavar "SOURCE" <> "s"
                    , paragraph "is updated."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ opt "config" "EXPR"
                , Options.fillSep
                    [ paragraph "Set configuration to"
                    , metavar "EXPR" <> ","
                    , "where"
                    , metavar "EXPR"
                    , paragraph "is a Dhall expression; if application fails\
                      \ to parse or typecheck the"
                    , metavar "EXPR"
                    , paragraph "it terminates with exit code"
                    , value "1" <> "."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ flag "config-typecheck"
                , Options.fillSep
                    [ paragraph "Typecheck the configuration and exit; exit\
                        \ code"
                    , value "0"
                    , paragraph "is used on success and exit code"
                    , value "1"
                    , paragraph "on failure to typecheck."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ flag "config-print-type"
                , Options.fillSep
                    [ paragraph "Print Dhall type of configuration accepted by\
                        \ the application to standard output and terminate\
                        \ with exit code"
                    , value "0" <> "."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ list [flag "version", shortFlag 'v']
                , Options.fillSep
                    [ paragraph "Print version information to standard output\
                        \ and terminate with exit code"
                    , value "0" <> "."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ list [flag "help", shortFlag 'h']
                , Options.fillSep
                    [ paragraph "Print help information to standard output and\
                        \ terminate with exit code"
                    , value "0" <> "."
                    ]
                ]
            ]
        <> globalOptions
        )
      where
        PrettyUtils
            { ellipsis
            , flag
            , list
            , metavar
            , opt
            , paragraph
            , shortFlag
            , shortOpt
            , value
            } = prettyUtils

        globalOptions :: [Options.Doc]
        globalOptions = case programName of
            StandaloneApplication _command ->
                []
            CommandWrapperSubcommand toolset _subcommand ->
                [ ""
                , Options.nest 4 $ Options.vsep
                    [ metavar "GLOBAL_OPTIONS"
                    , Options.fillSep
                        [ paragraph "See output of"
                        , Options.squotes
                            (value (Options.string toolset <+> "help"))
                            <> "."
                        ]
                    ]
                ]

data PrettyUtils = PrettyUtils
    { list :: [Options.Doc] -> Options.Doc
    , flagDoc :: Options.Doc -> Options.Doc
    , flag :: String -> Options.Doc
    , shortFlag :: Char -> Options.Doc
    , opt :: String -> String -> Options.Doc
    , shortOpt :: Char -> String -> Options.Doc
    , paragraph :: String -> Options.Doc
    , metavar :: Options.Doc -> Options.Doc
    , value :: Options.Doc -> Options.Doc
    , alt :: [Options.Doc] -> Options.Doc
    , ellipsis :: Options.Doc
    }

-- | Construct pretty printing utilities based on configuration.
mkPrettyUtils
    ::  ( (Options.Doc -> Options.Doc)
        -> (Options.Doc -> Options.Doc)
        -> Options.Doc -> Options.Doc
        )
    -> PrettyUtils
mkPrettyUtils colour =
    let list :: [Options.Doc] -> Options.Doc
        list = Options.encloseSep "" "" ", "

        flagDoc :: Options.Doc -> Options.Doc
        flagDoc = colour Options.dullgreen Options.bold

        flag :: String -> Options.Doc
        flag s = flagDoc (Options.string ("--" <> s))

        shortFlag :: Char -> Options.Doc
        shortFlag c = flagDoc (Options.string ['-', c])

        opt :: String -> String -> Options.Doc
        opt o v = flag o <> "=" <> metavar (Options.string v)

        shortOpt :: Char -> String -> Options.Doc
        shortOpt o v = shortFlag o <> " " <> metavar (Options.string v)

        paragraph :: String -> Options.Doc
        paragraph = Options.fillSep . fmap Options.string . String.words

        metavar :: Options.Doc -> Options.Doc
        metavar = colour Options.dullgreen Options.underline

        value :: Options.Doc -> Options.Doc
        value = colour Options.magenta Options.underline

        alt :: [Options.Doc] -> Options.Doc
        alt = \case
            [] -> ""
            (d : ds) -> d <> mconcat (List.zipWith (<>) (List.repeat "|") ds)

        ellipsis :: Options.Doc
        ellipsis = Options.brackets "..."

    in  PrettyUtils{..}

data Shell = Bash | Fish | Zsh

completer :: Version -> Configuration -> Shell -> Maybe Word -> [Text] -> IO ()
completer version config _shell index words
  | previousOneOf ["--platform", "-p"] =
        completePlatform config CompletionQuery
            { prefix = ""
            , word = current
            }

  | previousOneOf ["--language", "-L"] =
        completeLanguage config CompletionQuery
            { prefix = ""
            , word = current
            }

  | previousOneOf ["--source", "-s"] =
        completeSource config CompletionQuery
            { prefix = ""
            , word = current
            }

  | previous == Just "--config" =
        completeConfig version config CompletionQuery
            { prefix = ""
            , word = current
            }

  | Just ('-', _) <- Text.uncons current = if
      | "--platform=" `Text.isPrefixOf` current ->
            completePlatform config CompletionQuery
                { prefix = "--platform="
                , word = current
                }

      | "--language=" `Text.isPrefixOf` current ->
            completeLanguage config CompletionQuery
                { prefix = "--language="
                , word = current
                }

      | "--source=" `Text.isPrefixOf` current ->
            completeSource config CompletionQuery
                { prefix = "--source="
                , word = current
                }

      | "--config=" `Text.isPrefixOf` current ->
            completeConfig version config CompletionQuery
                { prefix = "--config="
                , word = current
                }

        -- Value of `current` is the first option on the command-line.
      | null before ->
            for_ (prefixMatch current topLevelOptions) \completion ->
                printCompletion Completion{prefix = "", completion}

        -- These options mean that nothing else should be completed.
      | hadBeforeOneOf topLevelTerminalOptions ->
            pure ()

      | hadBefore "--config-typecheck" -> do
            let possibilities :: [Text]
                possibilities = do
                    let opt = "--config="
                    opt <$ guard (not (hadBefore opt))

            for_ (prefixMatch current possibilities) \completion ->
                printCompletion Completion{prefix = "", completion}

      | hadBeforeOneOf ["--update", "-u"] -> do
            let possibilities :: [Text]
                possibilities = List.concat
                    [ ["--source=", "-s"]
                    , do
                        guard (not (hadBeforePrefix "--config="))
                        guard (not (hadBefore "--config"))
                        pure "--config="
                    ]

            for_ (prefixMatch current possibilities) \completion ->
                printCompletion Completion{prefix = "", completion}

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
                        guard (not (hadBeforePrefix "--language="))
                        guard (not (hadBeforeOneOf ["--language", "-L"]))
                        ["--language=", "-L"]
                    , do
                        guard (not (hadBeforePrefix "--platform="))
                        guard (not (hadBeforeOneOf ["--platform", "-p"]))
                        ["--platform=", "-p"]
                    ]

            for_ (prefixMatch current possibilities) \completion ->
                printCompletion Completion{prefix = "", completion}

  | hadBeforeOneOf notDefaultModeOptions =
      -- There are not arguments, only options in these modes.
      pure ()

  | otherwise =
        completeArgument config
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

data CompletionQuery = CompletionQuery
    { prefix :: Text
    , word :: Text
    }

completeLanguage :: Configuration -> CompletionQuery -> IO ()
completeLanguage config = withPrefix \word -> do
    cacheDir <- getCacheDirectory config
    Index.getIndexFile cacheDir >>= \case
        Nothing ->
            pure []
        Just indexFile ->
            SQLite.withConnection indexFile \connection ->
                List.sort <$> Index.getLocales connection word

completePlatform :: Configuration -> CompletionQuery -> IO ()
completePlatform config = withPrefix \word -> do
    let extra = prefixMatch word ["all"]
    cacheDir <- getCacheDirectory config
    list <- Index.getIndexFile cacheDir >>= \case
        Nothing ->
            pure []
        Just indexFile ->
            SQLite.withConnection indexFile \connection ->
                Index.getPlatforms connection word
    pure (List.sort (extra <> list))

completeSource :: Configuration -> CompletionQuery -> IO ()
completeSource Configuration{sources} = withPrefixPure \word ->
    prefixMatch word $ NonEmpty.toList sources <&> \Source{name} ->
        name

completeConfig :: Version -> Configuration -> CompletionQuery -> IO ()
completeConfig version _config = withPrefix \word -> do
    let env :: Text
        env = "env:"

        https :: Text
        https = "https://"

        remoteImport :: [Text]
        remoteImport = ["http://", https]

        libs :: [Text]
        libs =
            let base = "https://raw.githubusercontent.com/trskop/tldr-client/"
                    <> fromString (showVersion version)
                    <> "/dhall"
            in  [base <> "/config.dhall"]

        paths, incompletePaths :: [Text]
        paths = List.sort ["./", "../", "~/"]
        incompletePaths = [".", "..", "~"]

    if
      | env `Text.isPrefixOf` word -> do
            completions <- getEnvironment <&> fmap \(name, _value) ->
                env <> fromString name

            pure (prefixMatch word completions)

      | https `Text.isPrefixOf` word ->
            pure (prefixMatch word libs)

      | word `List.elem` incompletePaths ->
            pure (prefixMatch word paths)

      | any (`Text.isPrefixOf` word) paths -> do
            let completions = [] -- TODO

            pure (prefixMatch word completions)

      | otherwise ->
            pure (prefixMatch word (env : paths <> remoteImport))

completeArgument :: Configuration -> [Text] -> IO ()
completeArgument config@Configuration{prefixes} cmds = do
    cacheDir <- getCacheDirectory config
    possiblyIndexFile <- Index.getIndexFile cacheDir
    for_ possiblyIndexFile \indexFile -> do
        list <- SQLite.withConnection indexFile \connection -> do
            let cmdPrefixLength =
                    -- ["cmd"] → length "" → 0
                    -- ["foo","bar","cmd"] → length "foo-bar-" → 8
                    maybe 0 (\cs -> sum (Text.length <$> cs) + length cs)
                        (initMay cmds)

                cmd = Text.intercalate "-" cmds

            if null prefixes
                then
                    fmap (Text.drop cmdPrefixLength)
                        <$> Index.getCommands connection cmd
                else
                    concat <$> for prefixes \prefix -> do
                        let cmd' :: Text
                            cmd' = prefix <> "-" <> cmd

                            -- "${prefix}-" → length prefix + 1
                            prefixLength :: Int
                            prefixLength = Text.length prefix + 1

                        fmap (Text.drop (prefixLength + cmdPrefixLength))
                            <$> Index.getCommands connection cmd'

        for_ (List.sort list) \completion ->
            printCompletion Completion{prefix = "", completion}

prefixMatch :: Text -> [Text] -> [Text]
prefixMatch prefix options =
    List.sort (List.filter (prefix `Text.isPrefixOf`) options)

withPrefixPure :: (Text -> [Text]) -> CompletionQuery -> IO ()
withPrefixPure f = withPrefix (pure . f)

withPrefix :: (Text -> IO [Text]) -> CompletionQuery -> IO ()
withPrefix f CompletionQuery{..} = do
    completions <- f (Text.drop (Text.length prefix) word)
    for_ completions \completion ->
        printCompletion Completion{prefix, completion}

data Completion = Completion
    { prefix :: Text
    , completion :: Text
    }

printCompletion :: Completion -> IO ()
printCompletion Completion{..} = do
    Text.putStr prefix
    Text.putStrLn completion
