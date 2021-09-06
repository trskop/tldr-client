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

import Prelude (maxBound, minBound)

import Control.Applicative (pure)
import Control.Monad (guard, when)
import Data.Bool (not)
import qualified Data.Char as Char (toLower)
import Data.Function (($), (.))
import Data.Functor ((<$), (<$>))
import qualified Data.List as List (intercalate)
import Data.Maybe (Maybe(Just), maybe)
import Data.Semigroup ((<>))
import Data.String (String)
import qualified Data.String as String (unwords)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, hPutStrLn, stderr)
import Text.Show (show)

import Control.Monad.Except (throwError)
import qualified Data.CaseInsensitive as CI (mk)
import Data.Output.Colour (ColourOutput)
import qualified Data.Output.Colour as ColourOutput
    ( ColourOutput(Always, Auto, Never)
    , parse
    , toString
    )
import Data.Text (Text)
import qualified Data.Text as Text (null, unpack)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (parse)
import System.Directory (XdgDirectory(XdgConfig, XdgData), getXdgDirectory)
import System.Environment.Parser (ParseEnv, ParseEnvError(..), parseEnvIO, var)
import System.FilePath ((<.>), (</>))

import TldrClient.Client (Action(..), client)
import TldrClient.Configuration
    ( Source(Source, format, location, name)
    , SourceFormat(TldrPagesWithoutIndex)
    , SourceLocation(Local)
    , decodeSubcommandConfiguration
    , mkDefConfiguration
    )
import qualified TldrClient.Options as Options (Params(..), completer, parse)

import Paths_tldr_client (version)


main :: IO ()
main = do
    Environment{..} <- parseEnvironment
    let toolset = Text.unpack toolsetName
        subcommand = Text.unpack subcommandName
    (config, action) <- Options.parse Options.Params
        { version
        , colourOutput
        , verbosity
        , programName = toolset <> " " <> subcommand
        , configFile
        , configurationExpression
        , decoder = decodeSubcommandConfiguration verbosity colourOutput
        , mkDefault = mkDefConfiguration $ Just Source
            { name = toolsetName <> "-pages"
            , format = TldrPagesWithoutIndex
            , location =
                Local (dataDirectory </> "tldr" </> "pages" </> toolset)
            }
        , runCompletion = Options.completer
        }
    client config case action of
        Render platformOverride localeOverride sourcesOverride commands ->
            Render platformOverride localeOverride sourcesOverride
                (toolset : commands)

        List platformOverride localeOverride sourcesOverride prefix ->
            List platformOverride localeOverride sourcesOverride
                (toolset : prefix)

        Update sourcesOverride ->
            Update sourcesOverride

        ClearCache platformOverride localeOverride sourcesOverride ->
            ClearCache platformOverride localeOverride sourcesOverride

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

parseEnvironment :: IO Environment
parseEnvironment = do
    dataDirectory <- getXdgDirectory XdgData ""
    env@Environment{configFile} <- parseEnvIO () dieEnvError do
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
