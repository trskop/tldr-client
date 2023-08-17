-- |
-- Module:      Main
-- Description: Client for tldr-pages
-- Copyright:   (c) 2021-2023 Peter Trško
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

import Control.Applicative (pure)
import Control.Monad (when)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.String (String)
import System.Environment (getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, hPutStrLn, stderr)
import Text.Show (Show, show)

import Data.Output.Colour (ColourOutput)
import Data.Output.Colour qualified as ColourOutput
    ( ColourOutput(Auto)
    , noColorEnvVar
    )
import Data.Text (Text)
import Data.Text qualified as Text (unpack)
import Data.Verbosity (Verbosity)
import Data.Verbosity qualified as Verbosity (Verbosity(Annoying, Normal))
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)
import System.Environment.Parser (ParseEnvError(..), optionalVar, parseEnvIO)

import TldrClient.Client (client)
import TldrClient.Configuration
    ( decodeStandaloneConfiguration
    , mkDefConfiguration
    )
import TldrClient.Options qualified as Options
    ( Params(..)
    , ProgramName(StandaloneApplication)
    , completer
    , parse
    )

import Paths_tldr_client (version)


main :: IO ()
main = do
    env@Environment{..} <- parseEnvironment
    when (verbosity >= Verbosity.Annoying) do
        hPutStrLn stderr (programName <> ": Debug: " <> show env)
    (config, action) <- Options.parse Options.Params
        { version
        , colourOutput
        , verbosity
        , programName = Options.StandaloneApplication programName
        , configFile
        , configurationExpression
        , decoder = decodeStandaloneConfiguration
        , mkDefault = mkDefConfiguration Nothing
        , runCompletion = Options.completer version
        }
    client config action

data Environment = Environment
    { programName :: String
    , configFile :: FilePath
    , configurationExpression :: Maybe Text
    , verbosity :: Verbosity
    , colourOutput :: ColourOutput
    }
  deriving stock (Show)

parseEnvironment :: IO Environment
parseEnvironment = do
    programName <- getProgName
    env@Environment{configFile} <- parseEnvIO () (dieEnvError programName) do
        colourOutput <- fromMaybe ColourOutput.Auto <$> do
            ColourOutput.noColorEnvVar
        configurationExpression <- optionalVar "TLDR_CONFIG"
        pure Environment
            { configFile = "tldr/config.dhall"
            , verbosity = Verbosity.Normal
            , ..
            }

    absoluteConfigFile <- getXdgDirectory XdgConfig configFile
    pure env{configFile = absoluteConfigFile}
  where
    dieEnvError :: String -> ParseEnvError -> IO a
    dieEnvError programName err = do
        hPutStrLn stderr $ programName <> ": Error: " <> case err of
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
