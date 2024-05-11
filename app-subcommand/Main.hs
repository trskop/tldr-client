-- |
-- Module:      Main
-- Description: Client for tldr-pages
-- Copyright:   (c) 2021-2024 Peter Trško
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

import Prelude (maxBound, minBound, )

import Control.Applicative (pure, )
import Control.Monad (guard, when, )
import Data.Bool (Bool(False), not, )
import Data.Char qualified as Char (toLower, )
import Data.Foldable (elem, )
import Data.Function (($), (.), )
import Data.Functor ((<$), (<$>), )
import Data.List qualified as List (intercalate, )
import Data.Maybe (Maybe(Just), maybe, )
import Data.Ord ((>=), )
import Data.Semigroup ((<>), )
import Data.String (String, )
import Data.String qualified as String (unwords, )
import System.Exit (ExitCode(ExitFailure), exitWith, )
import System.IO (FilePath, IO, hPutStrLn, stderr, stdout, )
import Text.Show (Show, show, )

import Control.Monad.Except (throwError, )
import Data.CaseInsensitive qualified as CI (mk, )
import Data.Output.Colour (ColourOutput, )
import Data.Output.Colour qualified as ColourOutput
    ( ColourOutput(Always, Auto, Never)
    , parse
    , toString
    , )
import Data.Text (Text, )
import Data.Text qualified as Text (null, unpack, )
import Data.Verbosity (Verbosity, )
import Data.Verbosity qualified as Verbosity (Verbosity(Annoying), parse, )
import System.Directory (XdgDirectory(XdgConfig, XdgData), getXdgDirectory, )
import System.Environment.Parser (ParseEnv, ParseEnvError(..), parseEnvIO, var, )
import System.FilePath ((<.>), (</>), )

import TldrClient.Client (InputOutput(..), client, )
import TldrClient.Configuration
    ( Source(Source, format, location, name)
    , SourceFormat(TldrPagesWithoutIndex)
    , SourceLocation(Local)
    , decodeSubcommandConfiguration
    , mkDefConfiguration
    , )
import TldrClient.Configuration qualified as Configuration
    ( Configuration(colourOutput, prefixes, verbosity)
    , )
import TldrClient.Options qualified as Options
    ( CompleterParams(..)
    , Params(..)
    , ProgramName(CommandWrapperSubcommand)
    , completer
    , parse
    , )

import Paths_tldr_client (version, )


main :: IO ()
main = do
    env@Environment{..} <- parseEnvironment inputOutput
    let toolset = Text.unpack toolsetName
        subcommand = Text.unpack subcommandName
    when (verbosity >= Verbosity.Annoying) do
        hPutStrLn inputOutput.errorOutput
            (toolset <> " " <> subcommand <> ": Debug: " <> show env)
    (config, action) <- Options.parse Options.Params
        { version
        , colourOutput
        , verbosity = Just verbosity
        , programName = Options.CommandWrapperSubcommand toolset subcommand
        , configFile
        , configurationExpression
        , decoder = decodeSubcommandConfiguration verbosity colourOutput
        , encoder = show -- TODO
        , mkDefault =
            mkDefConfiguration False{-is not standalone app-} $ Just Source
                { name = toolsetName <> "-pages"
                , format = TldrPagesWithoutIndex
                , location =
                    Local (dataDirectory </> "tldr" </> "pages" </> toolset)
                }
        , updateConfig = \updateVerbosity updateColourOutput cfg -> cfg
            { Configuration.verbosity = updateVerbosity cfg.verbosity
            , Configuration.colourOutput = updateColourOutput cfg.colourOutput
            , Configuration.prefixes =
                if toolsetName `elem` cfg.prefixes
                    then cfg.prefixes
                    else cfg.prefixes <> [toolsetName]
            }
        , runCompletion = \cfg handle shell index words ->
            Options.completer Options.CompleterParams{version, config = cfg, ..}
        , inputOutput
        }
    client config inputOutput action
  where
    inputOutput = InputOutput
        { errorOutput = stderr
        , standardOutput = stdout
        }

data Environment = Environment
    { toolsetExe :: FilePath
    , protocolVersion :: Text
    , toolsetName :: Text
    , subcommandName :: Text
    , configFile :: FilePath
    , configurationExpression :: Maybe Text
    , verbosity :: Verbosity
    , colourOutput :: ColourOutput
    , dataDirectory :: FilePath
    }
  deriving stock (Show)

parseEnvironment :: InputOutput -> IO Environment
parseEnvironment InputOutput{errorOutput} = do
    dataDirectory <- getXdgDirectory XdgData ""
    env <- parseEnvIO () dieEnvError do
        toolsetExe <- Text.unpack <$> nonEmptyVar "COMMAND_WRAPPER_EXE"
        protocolVersion <- nonEmptyVar "COMMAND_WRAPPER_VERSION"
        toolsetName <- nonEmptyVar "COMMAND_WRAPPER_NAME"
        subcommandName <- nonEmptyVar "COMMAND_WRAPPER_SUBCOMMAND"
        configurationExpression <- do
            expr <- var "COMMAND_WRAPPER_CONFIG"
            pure do
                expr <$ guard (not (Text.null expr))
        verbosity <- do
            let name = "COMMAND_WRAPPER_VERBOSITY"
            value <- nonEmptyVar name
            maybe (verbosityParseError name value) pure do
                Verbosity.parse (CI.mk value)
        colourOutput <- do
            let name = "COMMAND_WRAPPER_COLOUR"
            value <- nonEmptyVar name
            maybe (colourOutputParseError name value) pure do
                ColourOutput.parse (CI.mk value)

        pure Environment
            -- TODO: This is basically just a guess and depends on this
            -- executable to not be renamed. Maybe we should not have it there
            -- at all.
            { configFile =
                let toolset = Text.unpack toolsetName
                    subcommand = Text.unpack subcommandName
                in  toolset </> "command-wrapper-" <> subcommand <.> "dhall"
            , ..
            }

    absoluteConfigFile <- getXdgDirectory XdgConfig env.configFile
    pure env{configFile = absoluteConfigFile}
  where
    dieEnvError :: ParseEnvError -> IO a
    dieEnvError err = do
        hPutStrLn errorOutput $ "Error: " <> case err of
            ParseEnvError name msg ->
                Text.unpack name <> ": " <> msg
            MissingEnvVarError name ->
                Text.unpack name <> ": Required environment variable missing."
            EmptyEnvVarError name ->
                Text.unpack name <> ": Value can not be empty:\
                    \ Environment variable is defined, but its value set to an\
                    \ empty string."
            ErrorMessage msg ->
                msg
            UnknownError ->
                "Failed to parse environment."
        exitWith (ExitFailure 2)

    nonEmptyVar :: Text -> ParseEnv context Text
    nonEmptyVar name = do
        value <- var name
        value <$ when (Text.null value) do
            definedButEmptyParseError name

    definedButEmptyParseError :: Text -> ParseEnv context a
    definedButEmptyParseError name = throwError
        (ParseEnvError name "Variable defined, but the value is empty.")

    colourOutputParseError :: Text -> Text -> ParseEnv context ColourOutput
    colourOutputParseError name value =
        throwError . ParseEnvError name $ String.unwords
            [ "Could not parse:"
            , show value <> ":"
            , "Expected one of:"
            , List.intercalate ", "
                ( ColourOutput.toString <$>
                    [ ColourOutput.Always
                    , ColourOutput.Auto
                    , ColourOutput.Never
                    ]
                )
                <> "."
            ]

    verbosityParseError :: Text -> Text -> ParseEnv context Verbosity
    verbosityParseError name value =
        throwError . ParseEnvError name $ String.unwords
            [ "Could not parse:"
            , show value <> ":"
            , "Expected one of:"
            , List.intercalate ", "
                [showVerbosity v | v <- [minBound .. maxBound]]
                <> "."
            ]

    showVerbosity :: Verbosity -> String
    showVerbosity v = Char.toLower <$> show v
