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

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Semigroup ((<>))
import Data.String (String)
import System.Environment (getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, hPutStrLn, stderr)

import Data.Output.Colour (ColourOutput)
import qualified Data.Output.Colour as ColourOutput
    ( ColourOutput(Auto)
    , noColorEnvVar
    )
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)
import System.Environment.Parser (ParseEnvError(..), optionalVar, parseEnvIO)

import TldrClient.Client (client)
import TldrClient.Configuration
    ( decodeStandaloneConfiguration
    , mkDefConfiguration
    )
import qualified TldrClient.Options as Options (Params(..), completer, parse)

import Paths_tldr_client (version)


main :: IO ()
main = do
    Environment{..} <- parseEnvironment
    -- TODO: Better error message when parsing environment variable(s) fails.
    (config, action) <- Options.parse Options.Params
        { version
        , colourOutput
        , verbosity
        , programName
        , configFile
        , configurationExpression
        , decoder = decodeStandaloneConfiguration
        , mkDefault = mkDefConfiguration Nothing
        , runCompletion = Options.completer
        }
    client config action

data Environment = Environment
    { programName :: String
    , configFile :: FilePath
    , configurationExpression :: Maybe Text
    , verbosity :: Verbosity
    , colourOutput :: ColourOutput
    }

parseEnvironment :: IO Environment
parseEnvironment = do
    programName <- getProgName
    env@Environment{configFile} <- parseEnvIO () dieEnvError do
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
    dieEnvError :: ParseEnvError -> IO a
    dieEnvError err = do
        hPutStrLn stderr $ "Error: " <> case err of
            ParseEnvError name msg ->
                Text.unpack name <> ": " <> msg
            MissingEnvVarError name ->
                Text.unpack name <> ": Required environment variable missing."
            ErrorMessage msg ->
                msg
            UnknownError ->
                "Failed to parse environment."
        exitWith (ExitFailure 2)
