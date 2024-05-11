{-# LANGUAGE StrictData #-}
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
    ( Params(..)
    , parse
    , ProgramName(..) -- re-exported

    -- * Completer
    , CompleterParams(..) -- re-exported
    , completer -- re-exported
    , )
  where

import Control.Applicative ((<**>), (<|>), pure, )
import Control.Exception (throwIO, )
import Control.Monad ((>>=), )
import Data.Bool (otherwise, )
import Data.Eq ((==))
import Data.Function (($), (.), const, id, )
import Data.Functor (($>), (<$>), (<&>), )
import Data.Int (Int, )
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe, )
import Data.Semigroup ((<>), )
import Data.String (String, )
import Data.Version (Version, makeVersion, )
import Data.Word (Word, )
import System.Environment (getArgs, )
import System.Exit
    ( ExitCode(ExitSuccess)
    , exitFailure
    , exitSuccess
    , exitWith
    , )
import System.IO (FilePath, Handle, IO, hIsTerminalDevice, hPutStrLn, )

import Data.Either.Validation qualified as Validation
    ( Validation(Failure, Success)
    , )
import Data.Output.Colour (ColourOutput, )
import Data.Output.Colour qualified as ColourOutput
    ( ColourOutput(Always, Auto, Never)
    , )
import Data.Text (Text, )
import Data.Text qualified as Text (unlines, )
import Data.Text.IO qualified as Text (hPutStrLn, )
import Data.Verbosity (Verbosity, )
import Data.Verbosity qualified as Verbosity (Verbosity(Normal), )
import Dhall qualified (Decoder(expected), input, inputFile, )
import Dhall.Version (dhallVersion, )
import Options.Applicative qualified as Options
    ( InfoMod
    , ParserHelp(helpBody, helpFooter, helpUsage)
    , ParserResult(CompletionInvoked, Failure, Success)
    , defaultPrefs
    , execFailure
    , execParserPure
    , footerDoc
    , fullDesc
    , header
    , helper
    , info
    , )
import Options.Applicative.Help ((<+>))
import Options.Applicative.Help qualified as Options (Doc, renderHelp, string, )
import Prettyprinter qualified (pretty, line)
import Prettyprinter.Render.Terminal qualified as Prettyprinter (hPutDoc, )
import System.Console.Terminal.Size as Terminal (Window(width), hSize, )
import System.Directory (doesFileExist, )

import Data.List.Compat (List, )
import TldrClient.Client qualified as Client
    ( Action
    , InputOutput(errorOutput, standardOutput)
    , )
import TldrClient.Configuration (shouldUseColours, )
import TldrClient.Options.Shell (Shell, )
import TldrClient.Options.Mode
    ( Mode(..)
    , ExecuteParams(..)
    , CompleteParams(..)
    , options
    , usage
    , optionsDoc
    , CompleterParams(..)
    , completer
    , )
import TldrClient.Options.PrettyUtils (PrettyUtils(value), mkPrettyUtils, )
import TldrClient.Options.ProgramName (ProgramName(..), )
import TldrClient.Options.ProgramName qualified as ProgramName (toString, )
import TldrClient.Version (VersionInfo(..), prettyVersionInfo, )


-- | Parameters passed to `parse` function to avoid ad-hoc function arguments
-- antipattern.
--
-- Keeping @config@ polymorphic forces us to push relevant logic to the caller.
-- That way it's not possible for the options-related code to use configuration
-- outside of what is provided. The reason for all of this is to make sure that
-- the code can operate correctly in different contexts, like standalone app
-- and CommandWrapper subcommand.
data Params config = Params
    { version :: Version
    -- ^ Application version printed in @--version@ mode.
    , colourOutput :: ColourOutput
    , verbosity :: Maybe Verbosity
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
    , updateConfig
        :: (Verbosity -> Verbosity)
        -> (ColourOutput -> ColourOutput)
        -> config
        -> config
    -- ^ Update configuration to reflect current user preferences.
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
    execOptionsParser inputOutput programName colourOutput configFile >>= \case
        Execute ExecuteParams{config = possiblyConfig, ..} ->
            (, action) . updateConfig' <$> parseConfig possiblyConfig

        Typecheck config -> do
            -- TODO: When there is no config file (mkDefault used) then we
            -- should probably complain.
            _ <- parseConfig config
            exitSuccess

        PrintType -> do
            printType inputOutput.standardOutput
            exitSuccess

        PrintConfig config -> do
            -- TODO: When there is no config file (mkDefault used) then we
            -- should probably complain.
            parseConfig config >>= printConfig inputOutput.standardOutput
            exitSuccess

        PrintVersion -> do
            Prettyprinter.hPutDoc inputOutput.standardOutput
                $ prettyVersionInfo VersionInfo
                    { clientVersion = version
                    , subcommandProtocolVersion = case programName of
                        StandaloneApplication{} -> Nothing
                        CommandWrapperSubcommand{} -> Just (makeVersion [1,0,0])
                    , tldrClientSpecificationVersion = makeVersion [2,2]
                    , dhallLibraryVersion = dhallVersion
                    }
            exitSuccess

        PrintCompletionInfo -> do
            Text.hPutStrLn inputOutput.standardOutput $ Text.unlines
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

        Complete CompleteParams{config = possiblyConfig, ..} -> do
            config <- updateConfig' <$> parseConfig possiblyConfig
            runCompletion config inputOutput.standardOutput shell index words
            exitSuccess
  where
    updateConfig' :: config -> config
    updateConfig' =
        let updateVerbosity :: Verbosity -> Verbosity
            updateVerbosity = maybe id const verbosity

            updateColourOutput :: ColourOutput -> ColourOutput
            updateColourOutput = case colourOutput of
                ColourOutput.Always -> const ColourOutput.Always
                ColourOutput.Auto   -> id
                ColourOutput.Never  -> const ColourOutput.Never

        in  updateConfig updateVerbosity updateColourOutput

    parseConfig :: Maybe Text -> IO config
    parseConfig commandLineExpr = do
        let decoder' = decoder programNameStr
        case commandLineExpr <|> configurationExpression of
            Just expr ->
                -- Configuration passed via command line option `--config=EXPR`
                -- or the one passed via configuration environment variable.
                -- Command line option has higher priority.
                Dhall.input decoder' expr

            Nothing -> do
                configExists <- doesFileExist configFile
                if configExists
                    then
                        Dhall.inputFile decoder' configFile
                    else
                        mkDefault
                            (fromMaybe Verbosity.Normal verbosity)
                            colourOutput
                            programNameStr

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
    programNameStr = ProgramName.toString programName

execOptionsParser
    :: Client.InputOutput
    -> ProgramName
    -> ColourOutput
    -> FilePath
    -> IO Mode
execOptionsParser inputOutput programName colourOutput configFile = do
    let parserInfo = Options.info (options <**> Options.helper) infoMod
    args <- getArgs
    case Options.execParserPure Options.defaultPrefs parserInfo args of
        Options.Success r ->
            pure r

        Options.Failure failure -> do
            let (help, exit, _) =
                    Options.execFailure failure
                        (ProgramName.toString programName)

                handle :: Handle
                handle =
                    -- ExitSuccess indicates that we are printing help
                    -- information and that there wasn't actually any options
                    -- parsing failure. This is how `--help|-h` is implemented.
                    if exit == ExitSuccess
                        then inputOutput.standardOutput
                        else inputOutput.errorOutput

            applyTerminalStyle <- do
                useColours <- shouldUseColours handle colourOutput
                isTerminal <- hIsTerminalDevice handle
                pure \applyColours applyDecoration doc -> if
                  | useColours -> applyColours doc
                  | isTerminal -> applyDecoration doc
                  | otherwise  -> doc
            cols <- Terminal.hSize handle <&> \case
                Nothing     -> 80
                Just window -> window.width
            let prettyUtils = mkPrettyUtils applyTerminalStyle \_ c -> c
            hPutStrLn handle (renderHelp prettyUtils cols help)
            exitWith exit

        Options.CompletionInvoked{} ->
            -- We don't use optparse-applicative command line completion.
            exitFailure
  where
    infoMod :: Options.InfoMod a
    infoMod = Options.fullDesc
        <> Options.header "Display and query tldr-pages."
        <> Options.footerDoc
            (Just $ footerDoc (mkPrettyUtils (\_ _ x -> x) \_ c -> c))

    footerDoc :: PrettyUtils -> Options.Doc
    footerDoc utils =
        "User configuration file is read from:"
        <+> utils.value (Options.string configFile)

    renderHelp :: PrettyUtils -> Int -> Options.ParserHelp -> String
    renderHelp prettyUtils cols help = Options.renderHelp cols help
        -- We want to set better help message iff the original
        -- help message piece is not empty. This way we are
        -- respecting options parser preferences.
        { Options.helpUsage =
            help.helpUsage $> usage prettyUtils programName
        , Options.helpBody =
            help.helpBody $> optionsDoc prettyUtils programName
        , Options.helpFooter =
            help.helpFooter $> footerDoc prettyUtils
        }
