{-# LANGUAGE StrictData #-}
-- |
-- Module:      TldrClient.Options.Mode
-- Description: Options parser, help messages, and command-line completer
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Options parser, help messages, and command-line completer.
--
-- It is important to keep help message, completer, and options parser in one
-- module. In most cases they will be all modified together and keeping them in
-- separate files can make it easier to introduce bugs.
--
-- Colour vs. no-colour rendering has been separated into `PrettyUtils`, see
-- "TldrClient.Options.PrettyUtils" module for more information.
module TldrClient.Options.Mode
    (
    -- * Options Parser
      Mode(..)
    , ExecuteParams(..)
    , CompleteParams(..)
    , options

    -- * Help Message
    , usage
    , optionsDoc

    -- * Completer
    , CompleterParams(..)
    , completer
    )
  where

import Prelude ((+), fromIntegral, )

import Control.Applicative ((<*>), (<|>), many, optional, pure, some, )
import Control.Monad ((>>=), guard, )
import Data.Bool (Bool(False), not, otherwise, )
import Data.Either (Either(Left, Right), )
import Data.Eq ((==), )
import Data.Foldable (any, concat, for_, length, null, sum, )
import Data.Function (($), (.), )
import Data.Functor ((<$), (<$>), (<&>), fmap, )
import Data.Int (Int, )
import Data.List qualified as List
    ( concat
    , elem
    , filter
    , intercalate
    , sort
    , take
    )
import Data.List.NonEmpty qualified as NonEmpty (toList, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Semigroup ((<>), )
import Data.String (String, fromString, )
import Data.Traversable (for, )
import Data.Version (Version, showVersion, )
import Data.Word (Word, )
import System.Environment (getEnvironment, )
import System.IO (FilePath, Handle, IO, stdin, )

import Data.Text (Text, )
import Data.Text qualified as Text
    ( drop
    , intercalate
    , isPrefixOf
    , length
    , uncons
    , )
import Data.Text.IO qualified as Text (hPutStr, hPutStrLn, )
import Database.SQLite.Simple qualified as SQLite (withConnection, )
import Options.Applicative qualified as Options
    ( Parser
    , auto
    , eitherReader
    , flag'
    , internal
    , long
    , metavar
    , option
    , short
    , strArgument
    , strOption
    , )
import Options.Applicative.Help ((<+>), )
import Options.Applicative.Help qualified as Options
    ( Doc
    , braces
    , brackets
    , fillSep
    , hang
    , nest
    , squotes
    , string
    , vsep
    , )
import Safe (atDef, initMay, lastDef, lastMay, )

import Data.List.Compat (List, )
import TldrClient.Client qualified as Client
    ( Action
        ( ClearCache
        , List
        , Render
        , RenderFile
        , Update
        )
    , ClearCeacheParams
        ( ClearCeacheParams
        , localeOverride
        , platformOverride
        , sourcesOverride
        )
    , ListParams
        ( ListParams
        , localeOverride
        , platformOverride
        , sourcesOverride
        )
    , RenderFileParams
        ( RenderFileParams
        , input
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
    , )
import TldrClient.Configuration
    ( Configuration(prefixes, sources)
    , Source(name)
    , getCacheDirectory
    , )
import TldrClient.Index qualified as Index
    ( getCommands
    , getIndexFile
    , getLocales
    , getPlatforms
    , )
import TldrClient.Locale (Locale, )
import TldrClient.Locale qualified as Locale (parse, )
import TldrClient.Options.Shell (Shell, )
import TldrClient.Options.Shell qualified as Shell (parse, )
import TldrClient.Platform (SomePlatform, )
import TldrClient.Platform qualified as Platform (parse, )
import TldrClient.Options.PrettyUtils
    ( PrettyUtils
        ( list
        , flag
        , shortFlag
        , opt
        , shortOpt
        , paragraph
        , metavar
        , value
        , alt
        , ellipsis
        )
    , )
import TldrClient.Options.ProgramName (ProgramName(..), )


-- {{{ Options ----------------------------------------------------------------

-- | Data type representing all the primary modes of operation of the
-- application. Only `Execute` data constructor represents the actual
-- tldr-pages client functionality.
data Mode
    = Execute ExecuteParams
    -- ^ We want to execute the application with the given configuration.
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
    | Complete CompleteParams
    -- ^ Do command-line completion.

data ExecuteParams = ExecuteParams
    { config :: Maybe Text
    -- ^ Configuration expression passed via a command-line option. `Nothing`
    -- indicates that there wasn't such option specified.
    --
    -- If the value is `Nothing` we need to read the environment variable or
    -- configuration file.
    , action :: Client.Action
    -- ^ Actual tldr-pages client action. Meta actions are handled by other
    -- constructors of the `Mode`.
    }

data CompleteParams = CompleteParams
    { config :: Maybe Text
    -- ^ Configuration expression passed via a command-line option. `Nothing`
    -- indicates that there wasn't such option specified.
    --
    -- If the value is `Nothing` we need to read the environment variable or
    -- configuration file.
    , shell :: Shell
    -- ^ Shell for which we are invoking command-line completion.
    , index :: Maybe Word
    -- ^ Index into the @words@ list (0-based, i.e. zero is the first item).
    -- Special cases are interpreted as follows:
    --
    -- * `Nothing` — complete last word in the @words@ list or empty string if
    --   the list is empty.
    --
    -- * @`Just` i@ where @i@ is out of bounds of @words@ list — complete empty
    --   string instead of a word from the @words@ list.
    --
    -- * @`Just` i@ — complete the word at the position @i@ in the @words@
    --   list.
    , words :: List Text
    -- ^ Invoked command-line parsed into individual words based on current
    -- shell's language.
    }

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
            <|> renderStdinFlag
            <|> renderFileOption
            )
        <*> optional configOption
        )
  where
    renderAction
        :: List String
        -> Maybe Locale
        -> Maybe (SomePlatform Text)
        -> List Text
        -> Maybe Text
        -> Mode
    renderAction commands localeOverride platformOverride sourcesOverride
      config = Execute ExecuteParams
        { config
        , action = Client.Render Client.RenderParams
            { command = List.intercalate "-" commands
            , localeOverride
            , platformOverride
            , sourcesOverride
            }
        }

    renderFileAction :: Either Handle FilePath -> Maybe Text -> Mode
    renderFileAction input config =
        Execute ExecuteParams
            { config
            , action = Client.RenderFile Client.RenderFileParams{input}
            }

    renderStdinFlag :: Options.Parser (Maybe Text -> Mode)
    renderStdinFlag = Options.flag' (renderFileAction (Left stdin))
        (Options.long "render-stdin")

    renderFileOption :: Options.Parser (Maybe Text -> Mode)
    renderFileOption = renderFileAction . Right <$> Options.option
        (Options.eitherReader parseFile)
        ( Options.long "render-file"
        <> Options.metavar "FILE"
        )
      where
        parseFile :: String -> Either String FilePath
        parseFile s
          | null s    = Left "FILE must be specified (must be non-empty string)"
          | otherwise = Right s

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
        listAction localeOverride platformOverride sourcesOverride config =
            Execute ExecuteParams
                { config
                , action = Client.List Client.ListParams
                    { localeOverride
                    , platformOverride
                    , sourcesOverride
                    }
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
        clearCacheAction localeOverride platformOverride sourcesOverride
          config = Execute ExecuteParams
            { config
            , action = Client.ClearCache Client.ClearCeacheParams
                { localeOverride
                , platformOverride
                , sourcesOverride
                }
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
        updateAction sourcesOverride config = Execute ExecuteParams
            { config
            , action = Client.Update Client.UpdateParams{sourcesOverride}
            }

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
            Complete CompleteParams{config, shell, index, words}

    indexOption :: Options.Parser Word
    indexOption = Options.option Options.auto
        ( Options.long "index"
        <> Options.metavar "INDEX"
        <> Options.internal
        )

    shellOption :: Options.Parser Shell
    shellOption = Options.option (Options.eitherReader Shell.parse)
        ( Options.long "shell"
        <> Options.metavar "SHELL"
        <> Options.internal
        )

    wordOption :: Options.Parser Text
    wordOption = Options.strOption
        ( Options.long "word"
        <> Options.metavar "WORD"
        <> Options.internal
        )

-- }}} Options ----------------------------------------------------------------

-- {{{ Help Messages ----------------------------------------------------------

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
    StandaloneApplication{} ->
        Nothing
    CommandWrapperSubcommand toolset subcommand ->
        Just $ Options.fillSep
            [ Options.string toolset
            , Options.brackets (utils.metavar "GLOBAL_OPTIONS")
            , "help"
            , Options.brackets (utils.flag "man")
            , Options.string subcommand
            ]

usage :: PrettyUtils -> ProgramName -> Options.Doc
usage utils programName = Options.nest 2 $ Options.vsep
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
                [ utils.opt "render-file" "FILE"
                , utils.flag "render-stdin"
                ]
            ]
        , Options.hang 2 $ Options.fillSep
            [ programName'
            , Options.braces $ utils.alt
                [ utils.flag "config-typecheck"
                , utils.flag "config-print-type"
--              , utils.flag "config-print" -- TODO: Implement properly
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
optionsDoc :: PrettyUtils -> ProgramName -> Options.Doc
optionsDoc utils programName = Options.nest 2 $ Options.vsep
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
                , utils.paragraph "then they are treated as one command with\
                    \ dashes in between. For example,"
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
                , utils.paragraph "this option can be used multiple times to\
                    \ specify multiple"
                , utils.metavar "PLATFORM" <> "s."
                , utils.paragraph "If not option is omitted then the platform\
                    \ the application is running on is used as a default."
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
                , utils.paragraph "this option can be used multiple times to\
                    \ specify multiple"
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
                    \ option can be used multiple times to specify multiple"
                , utils.metavar "SOURCE" <> "s."
                ]
            ]
        , ""
        , Options.nest 4 $ Options.vsep
            [ utils.list [utils.flag "list", utils.shortFlag 'l']
            , Options.fillSep
                [ utils.paragraph "Lists all the pages for the current\
                  \ platform to the standard output and exit with exit code"
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
            [ utils.list
                [ utils.opt "render-file" "FILE"
                , utils.flag "render-stdin"
                ]
            , Options.fillSep
                [ utils.paragraph "Render CommonMark"
                , utils.metavar "FILE"
                , utils.paragraph "or content passed to standard inputas a\
                    \ tldr page and exit with exit code"
                , utils.value "0" <> "."
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
                , utils.paragraph "is a Dhall expression; if application fails\
                  \ to parse or typecheck the"
                , utils.metavar "EXPR"
                , utils.paragraph "it terminates with exit code"
                , utils.value "1" <> "."
                ]
            ]
        , ""
        , Options.nest 4 $ Options.vsep
            [ utils.flag "config-typecheck"
            , Options.fillSep
                [ utils.paragraph "Typecheck the configuration and exit; exit\
                    \ code"
                , utils.value "0"
                , utils.paragraph "is used on success and exit code"
                , utils.value "1"
                , utils.paragraph "on failure to typecheck."
                ]
            ]
-- TODO: Implement properly.
--      , ""
--      , Options.nest 4 $ Options.vsep
--          [ utils.flag "config-print"
--          , Options.fillSep
--              [ utils.paragraph "Print configuration in the form of dhall\
--                  \ expression and terminate with exit code\
--              , utils.value "0" <> "."
--              ]
--          ]
        , ""
        , Options.nest 4 $ Options.vsep
            [ utils.flag "config-print-type"
            , Options.fillSep
                [ utils.paragraph "Print Dhall type of configuration accepted\
                    \ by the application to standard output and terminate with\
                    \ exit code"
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
                [ utils.paragraph "Print help information to standard output\
                    \ and terminate with exit code"
                , utils.value "0" <> "."
                ]
            ]
        ]
    <> globalOptions
    )
  where
    globalOptions :: List Options.Doc
    globalOptions = case programName of
        StandaloneApplication{} ->
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

-- }}} Help Messages ----------------------------------------------------------

-- {{{ Completer --------------------------------------------------------------

-- | See also `CompleteParams` documentation for details on most fields.
data CompleterParams = CompleterParams
    { version :: Version
    -- ^ Tldr-pages client version.
    , config :: Configuration
    , handle :: Handle
    -- ^ Handle into which the possible completions should be written to, one
    -- completion per line.
    , shell :: Shell
    -- ^ See documentation of the @shell@ field of `CompleteParams` record.
    , index :: Maybe Word
    -- ^ See documentation of the @index@ field of `CompleteParams` record.
    , words :: List Text
    -- ^ See documentation of the @words@ field of `CompleteParams` record.
    }

completer :: CompleterParams -> IO ()
completer CompleterParams{shell = _, ..}
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

  | previous == Just "--render-file" =
        -- TODO: File completion.
        pure ()

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

      | "--render-file=" `Text.isPrefixOf` current ->
        -- TODO: File completion.
        pure ()

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
--      , "--config-print" -- TODO: Implement properly
        , "--config-print-type"
        , "--config-typecheck"
        , "--update", "-u"
        , "--list", "-l"
        , "--clear-cache"
        , "--language=", "-L"
        , "--platform=", "-p"
        , "--source=", "-s"
        , "--config="
        , "--render-file="
        , "--render-stdin"
        ]

    topLevelTerminalOptions :: List Text
    topLevelTerminalOptions =
        [ "--help", "-h"
        , "--version", "-v"
--      , "--config-print" -- TODO: Implement properly
        , "--config-print-type"
        , "--render-file="
        , "--render-stdin"
        ]

    notDefaultModeOptions :: List Text
    notDefaultModeOptions =
        [ "--help", "-h"
        , "--version", "-v"
--      , "--config-print" -- TODO: Implement properly
        , "--config-print-type"
        , "--config-typecheck"
        , "--update", "-u"
        , "--list", "-l"
        , "--clear-cache"
        , "--render-file="
        , "--render-stdin"
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
prefixMatch prefix options' =
    List.sort (List.filter (prefix `Text.isPrefixOf`) options')

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

-- }}} Completer --------------------------------------------------------------
