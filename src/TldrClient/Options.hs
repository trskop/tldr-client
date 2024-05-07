-- |
-- Module:      TldrClient.Options
-- Description: Command-line options for a Tldr Client binary
-- Copyright:   (c) 2021-2024 Peter Trško
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
import Data.Foldable (any, concat, foldMap, for_, length, null, sum)
import Data.Function (($), (.))
import Data.Functor (($>), (<$), (<$>), (<&>), fmap)
import Data.Int (Int)
import Data.List qualified as List
    ( concat
    , elem
    , filter
    , intercalate
    , sort
    , take
    )
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.String qualified as String (words)
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
import System.IO (FilePath, Handle, IO, hIsTerminalDevice, hPutStrLn, )

import Data.CaseInsensitive (CI)
import Data.Either.Validation qualified as Validation
    ( Validation(Failure, Success)
    )
import Data.Output.Colour (ColourOutput)
import Data.Text (Text)
import Data.Text qualified as Text
    ( drop
    , intercalate
    , isPrefixOf
    , length
    , uncons
    , unlines
    )
import Data.Text.IO qualified as Text (hPutStr, hPutStrLn, )
import Data.Verbosity (Verbosity)
import Dhall qualified (Decoder(expected), input, inputFile)
import Dhall.Version (dhallVersion)
import Options.Applicative qualified as Options
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
import Options.Applicative.Help qualified as Options
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
import Prettyprinter qualified (pretty, line)
import Prettyprinter.Render.Terminal qualified as Prettyprinter (hPutDoc, )
import Safe (atDef, initMay, lastDef, lastMay)
import Database.SQLite.Simple qualified as SQLite (withConnection)
import System.Console.Terminal.Size as Terminal (Window(Window, width), hSize)
import System.Directory (doesFileExist)

import Data.List.Compat (List, )
import TldrClient.Client qualified as Client
    ( Action
        ( ClearCache
        , List
        , Render
        , Update
        )
    , ClearCeacheParams
        ( ClearCeacheParams
        , localeOverride
        , platformOverride
        , sourcesOverride
        )
    , InputOutput
        ( InputOutput
        , errorOutput
        , standardOutput
        )
    , ListParams
        ( ListParams
        , localeOverride
        , platformOverride
        , sourcesOverride
        )
    , RenderParams
        ( RenderParams
        , command
        , localeOverride
        , platformOverride
        , sourcesOverride
        )
    , UpdateParams
        ( UpdateParams
        , sourcesOverride
        )
    )
import TldrClient.Configuration
    ( Configuration(prefixes, sources)
    , Source(name)
    , getCacheDirectory
    , shouldUseColours
    )
import TldrClient.Index qualified as Index
    ( getCommands
    , getIndexFile
    , getLocales
    , getPlatforms
    )
import TldrClient.Locale (Locale, )
import TldrClient.Locale qualified as Locale (parse, )
import TldrClient.Platform (SomePlatform, )
import TldrClient.Platform qualified as Platform (parse, )
import TldrClient.Version (VersionInfo(..), prettyVersionInfo)


data Mode
    = Execute (Maybe Text) Client.Action
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
    | PrintConfig (Maybe Text)
    -- ^ Instead of doing anything just print the the configuration expression
    -- and exit. If the configuration is 'Nothing' we need to read the
    -- environment variable or configuration file.
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
programNameToDoc utils = \case
    StandaloneApplication command ->
        Options.string command
    CommandWrapperSubcommand toolset subcommand ->
        Options.fillSep
            [ Options.string toolset
            , Options.brackets (utils.metavar "GLOBAL_OPTIONS")
            , Options.string subcommand
            ]

-- | Generates usage line like:
--
-- > ${toolset} [GLOBAL_OPTIONS] help [--man] ${subcommand}
--
-- Returns `Nothing` when `ProgramName` is `StandaloneApplication` as the above
-- usage line is useful only for Command Wrapper subcommands.
programNameToHelpDoc :: PrettyUtils -> ProgramName -> Maybe Options.Doc
programNameToHelpDoc utils = \case
    StandaloneApplication _ ->
        Nothing
    CommandWrapperSubcommand toolset subcommand ->
        Just $ Options.fillSep
            [ Options.string toolset
            , Options.brackets (utils.metavar "GLOBAL_OPTIONS")
            , "help"
            , Options.brackets (utils.flag "man")
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
    , encoder :: config -> String
    -- ^ TODO: Switch to Dhall.Encoder config
    , mkDefault :: Verbosity -> ColourOutput -> String -> IO config
    -- ^ Construct default configuration if there is no configuration
    -- available. The `String` parameter is rendered
    -- @programName :: `ProgramName`@ field value.
    , runCompletion
        :: config -> Handle -> Shell -> Maybe Word -> List Text -> IO ()
    -- ^ Command-line completer.
    , inputOutput :: Client.InputOutput
    -- ^ These are here so that it's possible to parametrise the whole
    -- application and optins processing with I\/O handles. It's very useful
    -- for debugging to have this here even when the functionality is not
    -- exposed. For example, it's possible to modify the code to open a file
    -- and write the output there.
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
parse :: forall config. Params config -> IO (config, Client.Action)
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
            printType standardOutput
            exitSuccess

        PrintConfig config -> do
            -- TODO: When there is no config file (mkDefault used) then we
            -- should probably complain.
            parseConfig config >>= printConfig standardOutput
            exitSuccess

        PrintVersion -> do
            Prettyprinter.hPutDoc standardOutput $ prettyVersionInfo VersionInfo
                { clientVersion = version
                , subcommandProtocolVersion = case programName of
                    StandaloneApplication{} -> Nothing
                    CommandWrapperSubcommand{} -> Just (makeVersion [1,0,0])
                , tldrClientSpecificationVersion = makeVersion [2,2]
                , dhallLibraryVersion = dhallVersion
                }
            exitSuccess

        PrintCompletionInfo -> do
            Text.hPutStrLn standardOutput $ Text.unlines
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
            runCompletion config standardOutput shell index words
            exitSuccess
  where
    Client.InputOutput{errorOutput, standardOutput} = inputOutput

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

    printType :: Handle -> IO ()
    printType handle = case Dhall.expected (decoder programNameStr) of
        Validation.Success expr ->
            Prettyprinter.hPutDoc handle
                ( Prettyprinter.pretty expr
                <> Prettyprinter.line
                )

        Validation.Failure err ->
            -- This indicates a bug in the Decoder.
            throwIO err

    printConfig :: Handle -> config -> IO ()
    printConfig handle = hPutStrLn handle . encoder

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
                    handle =
                        if exit == ExitSuccess
                            then standardOutput
                            else errorOutput

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
    usage utils = Options.nest 2 $ Options.vsep
        (   [ "Usage:"
            , ""
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , configDoc, platformDoc, languageDoc, sourceDoc
                , utils.metavar "COMMAND"
                , Options.brackets
                    (utils.metavar "SUBCOMMAND" <+> utils.ellipsis)
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces
                    (utils.alt [utils.flag "list", utils.shortFlag 'l'])
                , configDoc, platformDoc, languageDoc, sourceDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces
                    (utils.alt [utils.flag "update", utils.shortFlag 'u'])
                , configDoc, platformDoc, languageDoc, sourceDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , utils.flag "clear-cache"
                , configDoc, platformDoc, languageDoc, sourceDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces $ utils.alt
                    [ utils.flag "config-typecheck"
                    , utils.flag "config-print-type"
                    ]
                , configDoc
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces
                    (utils.alt [utils.flag "version", utils.shortFlag 'v'])
                ]
            , Options.hang 2 $ Options.fillSep
                [ programName'
                , Options.braces
                    (utils.alt [utils.flag "help", utils.shortFlag 'h'])
                ]
            ]
        <> commandWrapperHelp
        )
      where
        programName' :: Options.Doc
        programName' = programNameToDoc utils programName

        configDoc :: Options.Doc
        configDoc = Options.brackets (utils.opt "config" "EXPR")

        platformDoc :: Options.Doc
        platformDoc = Options.brackets
            ( Options.braces
                ( utils.alt
                    [ utils.opt "platform" "PLATFORM"
                    , utils.shortOpt 'p' "PLATFORM"
                    ]
                )
            <+> utils.ellipsis
            )

        languageDoc :: Options.Doc
        languageDoc = Options.brackets
            ( Options.braces
                ( utils.alt
                    [ utils.opt "language" "LANGUAGE"
                    , utils.shortOpt 'L' "LANGUAGE"
                    ]
                )
            <+> utils.ellipsis
            )

        sourceDoc :: Options.Doc
        sourceDoc = Options.brackets
            ( Options.braces
                ( utils.alt
                    [utils.opt "source" "SOURCE", utils.shortOpt 's' "SOURCE"]
                )
            <+> utils.ellipsis
            )

        commandWrapperHelp :: List Options.Doc
        commandWrapperHelp =
            case programNameToHelpDoc utils programName of
                Nothing -> []
                Just doc -> [Options.hang 2 doc]

    options :: Options.Parser Mode
    options =
        versionFlag
        <|> printTypeFlag
        <|> completionInfoFlag
        <|> (   ( typecheckFlag
                <|> printConfigFlag
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
        :: List String
        -> Maybe Locale
        -> Maybe (SomePlatform Text)
        -> List Text
        -> Maybe Text
        -> Mode
    renderAction commands localeOverride platformOverride sourcesOverride cfg =
        Execute cfg $ Client.Render Client.RenderParams
            { command = List.intercalate "-" commands
            , localeOverride
            , platformOverride
            , sourcesOverride
            }

    configOption :: Options.Parser Text
    configOption = Options.strOption
        ( Options.long "config"
        <> Options.metavar "EXPR"
        )

    typecheckFlag :: Options.Parser (Maybe Text -> Mode)
    typecheckFlag = Options.flag' Typecheck (Options.long "config-typecheck")

    printTypeFlag :: Options.Parser Mode
    printTypeFlag = Options.flag' PrintType (Options.long "config-print-type")

    printConfigFlag :: Options.Parser (Maybe Text -> Mode)
    printConfigFlag = Options.flag' PrintConfig (Options.long "config-print")

    versionFlag :: Options.Parser Mode
    versionFlag = Options.flag' PrintVersion
        ( Options.long "version"
        <> Options.short 'v' -- Mandated by Tldr Client Specification
        )

    listFlag :: Options.Parser
        (  Maybe Locale
        -> Maybe (SomePlatform Text)
        -> List Text
        -> Maybe Text
        -> Mode
        )
    listFlag = Options.flag' listAction
        ( Options.long "list"
        <> Options.short 'l'
        )
      where
        listAction localeOverride platformOverride sourcesOverride cfg =
            Execute cfg $ Client.List Client.ListParams
                { localeOverride
                , platformOverride
                , sourcesOverride
                }

    clearCacheFlag :: Options.Parser
        (  Maybe Locale
        -> Maybe (SomePlatform Text)
        -> List Text
        -> Maybe Text
        -> Mode
        )
    clearCacheFlag = Options.flag' clearCacheAction
        ( Options.long "clear-cache"
        )
      where
        clearCacheAction localeOverride platformOverride sourcesOverride cfg =
            Execute cfg $ Client.ClearCache Client.ClearCeacheParams
                { localeOverride
                , platformOverride
                , sourcesOverride
                }

    platformOption :: Options.Parser (SomePlatform Text)
    platformOption = Options.option
        (Options.eitherReader (pure . Platform.parse . fromString))
        ( Options.long "platform"
        <> Options.short 'p'
        <> Options.metavar "PLATFORM"
        )

    languageOption :: Options.Parser Locale
    languageOption = Options.option
        (Options.eitherReader (Locale.parse . fromString))
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
        updateAction sourcesOverride cfg =
            Execute cfg (Client.Update Client.UpdateParams{sourcesOverride})

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
        :: Options.Parser
            (Shell -> Maybe Word -> List Text -> Maybe Text -> Mode)
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
    optionsDoc utils = Options.nest 2 $ Options.vsep
        (   [ "Options:"
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.metavar "COMMAND"
                    <+> Options.brackets
                        (utils.metavar "SUBCOMMAND" <+> utils.ellipsis)
                , Options.fillSep
                    [ utils.paragraph "Show pages for a"
                    , utils.metavar "COMMAND" <> "."
                    , "When"
                    , utils.metavar "COMMAND"
                    , utils.paragraph "is followed by"
                    , utils.metavar "SUBCOMMAND" <> "s"
                    , utils.paragraph "then they are treated as one command\
                        \ with dashes in between. For example,"
                    , "\"" <> utils.value "tldr git commit" <> "\""
                    , utils.paragraph "is the same as"
                    , "\"" <> utils.value "tldr git-commit" <> "\"."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.list
                    [ utils.opt "platform" "PLATFORM"
                    , utils.shortOpt 'p' "PLATFORM"
                    ]
                , Options.fillSep
                    [ utils.paragraph "Search or list pages for specified"
                    , utils.metavar "PLATFORM" <> ";"
                    , utils.paragraph "this option can be used multiple times\
                        \ to specify multiple"
                    , utils.metavar "PLATFORM" <> "s."
                    , utils.paragraph "If not option is omitted then the\
                        \ platform the application is running on is used as a\
                        \ default."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.list
                    [ utils.opt "language" "LANGUAGE"
                    , utils.shortOpt 'L' "LANGUAGE"
                    ]
                , Options.fillSep
                    [ utils.paragraph "Search/list pages written in"
                    , utils.metavar "LANGUAGE" <> ";"
                    , utils.paragraph "this option can be used multiple times\
                        \ to specify multiple"
                    , utils.metavar "LANGUAGE" <> "s."
                    , utils.paragraph "Overrides default language detection\
                        \ mechanism."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.list
                    [ utils.opt "source" "SOURCE"
                    , utils.shortOpt 's' "SOURCE"
                    ]
                , Options.fillSep
                    [ utils.paragraph "Show, list, or update cache only for\
                        \ specified"
                    , utils.metavar "SOURCE" <> "s;"
                    , utils.paragraph "by default all sources are used; this\
                        \ option can be used multiple times to specify\
                        \ multiple"
                    , utils.metavar "SOURCE" <> "s."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.list [utils.flag "list", utils.shortFlag 'l']
                , Options.fillSep
                    [ utils.paragraph "Lists all the pages for the current\
                      \ platform to the standard output and exit with exit\
                      \ code"
                    , utils.value "0" <> "."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.list [utils.flag "update", utils.shortFlag 'u']
                , Options.fillSep
                    [ utils.paragraph "Updates the offline cache of pages; if"
                    , Options.squotes (utils.opt "sources" "SOURCE")
                    , utils.paragraph "is specified only cache for those page"
                    , utils.metavar "SOURCE" <> "s"
                    , utils.paragraph "is updated."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.flag "clear-cache"
                , Options.fillSep
                    [ utils.paragraph "Updates the offline cache of pages; if"
                    , Options.squotes (utils.opt "sources" "SOURCE")
                    , utils.paragraph "is specified only cache for those page"
                    , utils.metavar "SOURCE" <> "s"
                    , utils.paragraph "is updated."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.opt "config" "EXPR"
                , Options.fillSep
                    [ utils.paragraph "Set configuration to"
                    , utils.metavar "EXPR" <> ","
                    , "where"
                    , utils.metavar "EXPR"
                    , utils.paragraph "is a Dhall expression; if application\
                      \ fails to parse or typecheck the"
                    , utils.metavar "EXPR"
                    , utils.paragraph "it terminates with exit code"
                    , utils.value "1" <> "."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.flag "config-typecheck"
                , Options.fillSep
                    [ utils.paragraph "Typecheck the configuration and exit;\
                        \ exit code"
                    , utils.value "0"
                    , utils.paragraph "is used on success and exit code"
                    , utils.value "1"
                    , utils.paragraph "on failure to typecheck."
                    ]
                ]
--          , ""
--          , Options.nest 4 $ Options.vsep
--              [ utils.flag "config-print"
--              , Options.fillSep
--                  [ utils.paragraph "Print configuration in the form of\
--                      \ dhall expression and terminate with exit code\
--                  , utils.value "0" <> "."
--                  ]
--              ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.flag "config-print-type"
                , Options.fillSep
                    [ utils.paragraph "Print Dhall type of configuration\
                        \ accepted by the application to standard output and\
                        \ terminate with exit code"
                    , utils.value "0" <> "."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.list [utils.flag "version", utils.shortFlag 'v']
                , Options.fillSep
                    [ utils.paragraph "Print version information to standard\
                        \ output and terminate with exit code"
                    , utils.value "0" <> "."
                    ]
                ]
            , ""
            , Options.nest 4 $ Options.vsep
                [ utils.list [utils.flag "help", utils.shortFlag 'h']
                , Options.fillSep
                    [ utils.paragraph "Print help information to standard\
                        \ output and terminate with exit code"
                    , utils.value "0" <> "."
                    ]
                ]
            ]
        <> globalOptions
        )
      where
        globalOptions :: List Options.Doc
        globalOptions = case programName of
            StandaloneApplication _command ->
                []
            CommandWrapperSubcommand toolset _subcommand ->
                [ ""
                , Options.nest 4 $ Options.vsep
                    [ utils.metavar "GLOBAL_OPTIONS"
                    , Options.fillSep
                        [ utils.paragraph "See output of"
                        , Options.squotes
                            (utils.value (Options.string toolset <+> "help"))
                            <> "."
                        ]
                    ]
                ]

data PrettyUtils = PrettyUtils
    { list :: List Options.Doc -> Options.Doc
    , flagDoc :: Options.Doc -> Options.Doc
    , flag :: String -> Options.Doc
    , shortFlag :: Char -> Options.Doc
    , opt :: String -> String -> Options.Doc
    , shortOpt :: Char -> String -> Options.Doc
    , paragraph :: String -> Options.Doc
    , metavar :: Options.Doc -> Options.Doc
    , value :: Options.Doc -> Options.Doc
    , alt :: List Options.Doc -> Options.Doc
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
    let list :: List Options.Doc -> Options.Doc
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

        alt :: List Options.Doc -> Options.Doc
        alt = \case
            [] -> ""
            (d : ds) -> d <> foldMap ("|" <>) ds

        ellipsis :: Options.Doc
        ellipsis = Options.brackets "..."

    in  PrettyUtils{..}

data Shell = Bash | Fish | Zsh

completer
    :: Version
    -> Configuration
    -> Handle
    -> Shell
    -> Maybe Word
    -> List Text
    -> IO ()
completer version config handle _shell index words
  | previousOneOf ["--platform", "-p"] =
        completePlatform config handle CompletionQuery
            { prefix = ""
            , word = current
            }

  | previousOneOf ["--language", "-L"] =
        completeLanguage config handle CompletionQuery
            { prefix = ""
            , word = current
            }

  | previousOneOf ["--source", "-s"] =
        completeSource config handle CompletionQuery
            { prefix = ""
            , word = current
            }

  | previous == Just "--config" =
        completeConfig version config handle CompletionQuery
            { prefix = ""
            , word = current
            }

  | Just ('-', _) <- Text.uncons current = if
      | "--platform=" `Text.isPrefixOf` current ->
            completePlatform config handle CompletionQuery
                { prefix = "--platform="
                , word = current
                }

      | "--language=" `Text.isPrefixOf` current ->
            completeLanguage config handle CompletionQuery
                { prefix = "--language="
                , word = current
                }

      | "--source=" `Text.isPrefixOf` current ->
            completeSource config handle CompletionQuery
                { prefix = "--source="
                , word = current
                }

      | "--config=" `Text.isPrefixOf` current ->
            completeConfig version config handle CompletionQuery
                { prefix = "--config="
                , word = current
                }

        -- Value of `current` is the first option on the command-line.
      | null before ->
            for_ (prefixMatch current topLevelOptions) \completion ->
                printCompletion handle Completion{prefix = "", completion}

        -- These options mean that nothing else should be completed.
      | hadBeforeOneOf topLevelTerminalOptions ->
            pure ()

      | hadBefore "--config-typecheck" -> do
            let possibilities :: List Text
                possibilities = do
                    let opt = "--config="
                    opt <$ guard (not (hadBefore opt))

            for_ (prefixMatch current possibilities) \completion ->
                printCompletion handle Completion{prefix = "", completion}

      | hadBeforeOneOf ["--update", "-u"] -> do
            let possibilities :: List Text
                possibilities = List.concat
                    [ ["--source=", "-s"]
                    , do
                        guard (not (hadBeforePrefix "--config="))
                        guard (not (hadBefore "--config"))
                        pure "--config="
                    ]

            for_ (prefixMatch current possibilities) \completion ->
                printCompletion handle Completion{prefix = "", completion}

        -- "--list", "-l", "--clear-cache", or default mode:
      | otherwise -> do
            let possibilities :: List Text
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
                printCompletion handle Completion{prefix = "", completion}

  | hadBeforeOneOf notDefaultModeOptions =
      -- There are not arguments, only options in these modes.
      pure ()

  | otherwise =
        completeArgument config handle
            ( List.filter (not . ("-" `Text.isPrefixOf`)) before
            <> [current]
            )
  where
    before :: List Text
    before
      | null words = []
      | otherwise  = maybe [] (\i -> List.take (fromIntegral i) words) index

    previous :: Maybe Text
    previous = lastMay before

    current :: Text
    current = maybe (lastDef "" words) (atDef "" words . fromIntegral) index

    previousOneOf :: List Text -> Bool
    previousOneOf opts = maybe False (`List.elem` opts) previous

    hadBefore :: Text -> Bool
    hadBefore opt = opt `List.elem` before

    hadBeforeOneOf :: List Text -> Bool
    hadBeforeOneOf opts = any (`List.elem` opts) before

    hadBeforePrefix :: Text -> Bool
    hadBeforePrefix prefix = any (prefix `Text.isPrefixOf`) before

    topLevelOptions :: List Text
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

    topLevelTerminalOptions :: List Text
    topLevelTerminalOptions =
        [ "--help", "-h"
        , "--version", "-v"
        , "--config-print-type"
        ]

    notDefaultModeOptions :: List Text
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

completeLanguage :: Configuration -> Handle -> CompletionQuery -> IO ()
completeLanguage config handle = withPrefix handle \word -> do
    cacheDir <- getCacheDirectory config
    Index.getIndexFile cacheDir >>= \case
        Nothing ->
            pure []
        Just indexFile ->
            SQLite.withConnection indexFile \connection ->
                List.sort <$> Index.getLocales connection word

completePlatform :: Configuration -> Handle -> CompletionQuery -> IO ()
completePlatform config handle = withPrefix handle \word -> do
    let extra = prefixMatch word ["all"]
    cacheDir <- getCacheDirectory config
    list <- Index.getIndexFile cacheDir >>= \case
        Nothing ->
            pure []
        Just indexFile ->
            SQLite.withConnection indexFile \connection ->
                Index.getPlatforms connection word
    pure (List.sort (extra <> list))

completeSource :: Configuration -> Handle -> CompletionQuery -> IO ()
completeSource config handle = withPrefixPure handle \word ->
    prefixMatch word $ NonEmpty.toList config.sources <&> (.name)

completeConfig :: Version -> Configuration -> Handle -> CompletionQuery -> IO ()
completeConfig version _config handle = withPrefix handle \word -> do
    let env :: Text
        env = "env:"

        https :: Text
        https = "https://"

        remoteImport :: List Text
        remoteImport = ["http://", https]

        libs :: List Text
        libs =
            let base = "https://raw.githubusercontent.com/trskop/tldr-client/"
                    <> fromString (showVersion version)
                    <> "/dhall"
            in  [base <> "/config.dhall"]

        paths, incompletePaths :: List Text
        paths = incompletePaths <&> (<> "/")
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

completeArgument :: Configuration -> Handle -> List Text -> IO ()
completeArgument config handle cmds = do
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

            if null config.prefixes
                then
                    fmap (Text.drop cmdPrefixLength)
                        <$> Index.getCommands connection cmd
                else
                    concat <$> for config.prefixes \prefix -> do
                        let cmd' :: Text
                            cmd' = prefix <> "-" <> cmd

                            -- "${prefix}-" → length prefix + 1
                            prefixLength :: Int
                            prefixLength = Text.length prefix + 1

                        fmap (Text.drop (prefixLength + cmdPrefixLength))
                            <$> Index.getCommands connection cmd'

        for_ (List.sort list) \completion ->
            printCompletion handle Completion{prefix = "", completion}

prefixMatch :: Text -> List Text -> List Text
prefixMatch prefix options =
    List.sort (List.filter (prefix `Text.isPrefixOf`) options)

withPrefixPure :: Handle -> (Text -> List Text) -> CompletionQuery -> IO ()
withPrefixPure handle f = withPrefix handle (pure . f)

withPrefix :: Handle -> (Text -> IO [Text]) -> CompletionQuery -> IO ()
withPrefix handle f CompletionQuery{..} = do
    completions <- f (Text.drop (Text.length prefix) word)
    for_ completions \completion ->
        printCompletion handle Completion{prefix, completion}

data Completion = Completion
    { prefix :: Text
    , completion :: Text
    }

printCompletion :: Handle -> Completion -> IO ()
printCompletion handle Completion{..} = do
    Text.hPutStr handle prefix
    Text.hPutStrLn handle completion
