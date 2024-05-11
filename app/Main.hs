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

import Control.Applicative (pure, )
import Control.Monad (when, )
import Data.Bool (Bool(True), )
import Data.Function (($), )
import Data.Functor ((<$>), )
import Data.Maybe (Maybe(Nothing), fromMaybe, )
import Data.Ord ((>=), )
import Data.Semigroup ((<>), )
import Data.String (String)
import System.Environment (getProgName, )
import System.Exit (ExitCode(ExitFailure), exitWith, )
import System.IO (FilePath, IO, hPutStrLn, stderr, stdout, )
import Text.Show (Show, show, )

import Data.Output.Colour (ColourOutput, )
import Data.Output.Colour qualified as ColourOutput
    ( ColourOutput(Auto)
    , noColorEnvVar
    , )
import Data.Text (Text, )
import Data.Text qualified as Text (unpack, )
import Data.Verbosity (Verbosity, )
import Data.Verbosity qualified as Verbosity (Verbosity(Annoying, Normal), )
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory, )
import System.Environment.Parser (ParseEnvError(..), optionalVar, parseEnvIO, )

import TldrClient.Client (InputOutput(..), client, )
import TldrClient.Configuration
    ( decodeStandaloneConfiguration
    , mkDefConfiguration
    , )
import TldrClient.Configuration qualified as Configuration
    ( Configuration(colourOutput, verbosity)
    , )
import TldrClient.Options qualified as Options
    ( CompleterParams(..)
    , Params(..)
    , ProgramName(StandaloneApplication)
    , completer
    , parse
    , )

import Paths_tldr_client (version, )


main :: IO ()
main = do
    env@Environment{..} <- parseEnvironment inputOutput
    when (fromMaybe Verbosity.Normal verbosity >= Verbosity.Annoying) do
        hPutStrLn inputOutput.errorOutput
            (programName <> ": Debug: " <> show env)
    (config, action) <- Options.parse Options.Params
        { version
        , colourOutput
        , verbosity
        , programName = Options.StandaloneApplication programName
        , configFile
        , configurationExpression
        , decoder = decodeStandaloneConfiguration
        , encoder = show -- TODO
        , mkDefault = mkDefConfiguration True{-is standalone app-} Nothing
        , updateConfig = \updateVerbosity updateColourOutput cfg -> cfg
            { Configuration.verbosity = updateVerbosity cfg.verbosity
            , Configuration.colourOutput = updateColourOutput cfg.colourOutput
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
    { programName :: String
    , configFile :: FilePath
    , configurationExpression :: Maybe Text
    , verbosity :: Maybe Verbosity
    , colourOutput :: ColourOutput
    }
  deriving stock (Show)

parseEnvironment :: InputOutput -> IO Environment
parseEnvironment InputOutput{errorOutput} = do
    programName <- getProgName
    env <- parseEnvIO () (dieEnvError programName) do
        colourOutput <- fromMaybe ColourOutput.Auto <$> do
            ColourOutput.noColorEnvVar
        configurationExpression <- optionalVar "TLDR_CONFIG"
        pure Environment
            { configFile = "tldr/config.dhall"
            , verbosity = Nothing
            , ..
            }

    absoluteConfigFile <- getXdgDirectory XdgConfig env.configFile
    pure env{configFile = absoluteConfigFile}
  where
    dieEnvError :: String -> ParseEnvError -> IO a
    dieEnvError programName err = do
        hPutStrLn errorOutput $ programName <> ": Error: " <> case err of
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
