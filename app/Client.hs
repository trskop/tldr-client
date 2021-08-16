-- |
-- Module:      TldrClient
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Client
    ( client
    , Action(..)
    , SomePlatform(..)
    , Platform(..)
    , parsePlatform
    )
  where

import Control.Applicative ((*>), pure)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool(True), (&&), not)
import qualified Data.Char as Char (toLower)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (/=))
import Data.Foldable (asum, concatMap, for_, length, null)
import Data.Function (($), (.), flip)
import Data.Functor ((<$), (<$>), (<&>))
import qualified Data.List as List (elem, filter, intercalate, notElem)
import qualified Data.List.NonEmpty as NonEmpty (head, toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Ord ((>), (>=))
import Data.Semigroup ((<>))
import Data.String (String)
import System.Exit (exitFailure)
import System.IO
    ( FilePath
    , IO
    , hFlush
    , hPrint
    , hPutStr
    , hPutStrLn
    , stderr
    , stdout
    )
import System.Info (os)
import Text.Show (Show)

import qualified Codec.Archive.Zip as Zip
    ( ZipOption(OptDestination)
    , extractFilesFromArchive
    , toArchive
    )
import Control.Lens (view)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.CaseInsensitive as CI (mk)
import qualified Data.LanguageCodes as LanguageCode (ISO639_1(EN))
--import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import qualified Data.Text as Text (intercalate, unpack)
import qualified Data.Verbosity as Verbosity (Verbosity(Annoying, Silent))
import Network.Wreq (get, responseBody)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removePathForcibly
    , renameDirectory
    )
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withTempDirectory)
import qualified Tldr (renderPage)
import qualified Tldr.Types as Tldr (ColorSetting({-NoColor,-} UseColor))

import Configuration
    ( Configuration(Configuration, sources, verbosity)
    , Source(Source, location, name)
    , SourceLocation(Local, Remote)
    , getCacheDirectory
    , getLocales
    )
import Locale (Locale(..), localeToText)


data Action
    = Render (Maybe SomePlatform) (Maybe Locale) [Text] [String]
    | List (Maybe SomePlatform) (Maybe Locale) [Text]
    | Update [Text]
  deriving stock (Eq, Show)

data SomePlatform
    = AllPlatforms
    | KnownPlatform Platform
    | OtherPlatform String
  deriving stock (Eq, Show)

-- | List of platforms as they are currently supported by tldr-pages, see
-- [github.com/tldr-pages/tldr](https://github.com/tldr-pages/tldr).
data Platform
    = Android
    | Linux
    | Osx
    | Sunos
    | Windows
  deriving stock (Eq, Show)

parsePlatform :: String -> Either String SomePlatform
parsePlatform s = case CI.mk s of
    "all" -> Right AllPlatforms
    "android" -> Right (KnownPlatform Android)
    "linux" -> Right (KnownPlatform Linux)
    "macos" -> Right (KnownPlatform Osx)
    "osx" -> Right (KnownPlatform Osx)
    "sunos" -> Right (KnownPlatform Sunos)
    "windows" -> Right (KnownPlatform Windows)
    _ ->
        -- TODO: Should there be some kind of restriction on the name?
        Right (OtherPlatform (Char.toLower <$> s))

client :: Configuration -> Action -> IO ()
client config@Configuration{sources, verbosity} action = do
    when (verbosity >= Verbosity.Annoying) do
        -- TODO: Find a nicer way to print these:
        hPutStr stderr "DEBUG: " *> hPrint stderr config
        hPutStr stderr "DEBUG: " *> hPrint stderr action

    cacheDirectory <- getCacheDirectory config
    when (verbosity >= Verbosity.Annoying) do
        hPutStrLn stderr ("DEBUG: Cache directory: " <> cacheDirectory)

    case action of
        Render platformOverride localeOverride sourcesOverride commands -> do
            let platform = fromMaybe currentPlatform platformOverride

                sourcesToUse = if null sourcesOverride
                    then allSources
                    else -- TODO: These may not necessarily exist. We need to
                         -- check that they are members of allSources.
                         Text.unpack <$> sourcesOverride

                allSources = NonEmpty.toList sources <&> \Source{name} ->
                    Text.unpack name

                command = List.intercalate "-" commands

            locales <- getLocales config localeOverride

            -- TODO: This is not how we want to do things in the future.
            -- Instead of finding potential paths we want to do a lookup in an
            -- index file/DB that will give us all the options that we have.
            -- That way we can also improve UX.
            let paths = pageCandidatePaths cacheDirectory sourcesToUse platform
                    locales command

            when (verbosity >= Verbosity.Annoying) do
                hPutStrLn stderr "DEBUG: Looking for page in these paths:"
                for_ paths \path ->
                    hPutStrLn stderr ("  " <> path)

            possiblyPath <- runMaybeT . asum $ paths <&> \path -> do
                fileExists <- liftIO (doesFileExist path)
                path <$ guard fileExists

            case possiblyPath of
                Nothing -> do
                    when (verbosity > Verbosity.Silent) do
                        hPutStrLn stderr
                            ( "ERROR: Unable to find a page for command: "
                            <> command
                            )
                    exitFailure
                Just path -> do
                    when (verbosity >= Verbosity.Annoying) do
                        hPutStrLn stderr ("DEBUG: Page found: " <> path)
                    Tldr.renderPage path stdout Tldr.UseColor

        List _platformOverride _localeOverride _sources ->
            -- TODO: Implement this!
            pure ()

        Update sourcesOverride -> do
            let sourcesToFetch :: [Source]
                sourcesToFetch = if null sourcesOverride
                    then NonEmpty.toList sources
                    else List.filter
                            (\Source{name} -> name `List.elem` sourcesOverride)
                            (NonEmpty.toList sources)

                -- If `sourcesOverride` has been specified we need to check
                -- that they all match with what we have in configuration.
                haveSpecifiedUnknownSource :: Bool
                haveSpecifiedUnknownSource =
                    not (null sourcesOverride)
                    -- The reason why the following line works is because we
                    -- use the `sourcesOverride` in the filter above. If the
                    -- code changes in any way this assumption has to be
                    -- revisited.
                    && length sourcesToFetch /= length sourcesOverride

            when (haveSpecifiedUnknownSource && verbosity > Verbosity.Silent) do
                let sourceNames :: [Text]
                    sourceNames = sourcesToFetch <&> \Source{name} -> name

                    unknownSources :: String
                    unknownSources =
                        Text.unpack . Text.intercalate ", "
                        $ List.filter (`List.notElem` sourceNames)
                            sourcesOverride

                hPutStrLn stderr
                    ( "WARNING: Unknown page sources specified on command\
                    \ line: " <> unknownSources
                    )

            -- Since configuration contains non-empty list this can happen only
            -- if only unknown sources were specified on the command line.
            when (null sourcesToFetch) do
                when (verbosity > Verbosity.Silent) do
                    let sourceNames :: String
                        sourceNames =
                            Text.unpack . Text.intercalate ", "
                            $ NonEmpty.toList sources <&> \Source{name} -> name

                    hPutStrLn stderr
                        ( "ERROR: No known (configured) page source was\
                        \ specified on the command line. Configured sources\
                        \ are: " <> sourceNames
                        )

                exitFailure

            for_ sourcesToFetch \source@Source{name} -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn stdout ("Updating '" <> Text.unpack name <> "'…")
                updateCache config cacheDirectory source

updateCache :: Configuration -> FilePath -> Source -> IO ()
updateCache _ _ Source{location = Local _} = pure ()
updateCache
  Configuration{verbosity}
  cacheDir
  Source{name, location = Remote urls} = do
    createDirectoryIfMissing True cacheDir
    when (verbosity >= Verbosity.Annoying) do
        hPutStrLn stderr
            ( "DEBUG: Target directory for '" <> sourceName <> "': "
            <> targetDir
            )

    withTempDirectory cacheDir sourceName \dir -> do
        when (verbosity > Verbosity.Silent) do
            hPutStr stdout
                ("  Downloading '" <> sourceName <> "' (" <> url <> ")… ")
            hFlush stdout

        -- TODO: At the moment we only try the primary URL, however, we should
        -- try the mirrors if we fail to download the primary URL.
        httpResponse <- try (get url)
        case httpResponse of
            Left e -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn stdout "failed"
                when (verbosity >= Verbosity.Annoying) do
                    hPutStrLn stderr
                        ( "ERROR: Download of " <> url <> " failed with: "
                        <> displayException @SomeException e
                        )

            Right (view responseBody -> body) -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn stdout "success"

                unpack body dir
  where
    sourceName :: String
    sourceName = Text.unpack name

    targetDir :: FilePath
    targetDir = cacheDir </> sourceName

    url :: String
    url = NonEmpty.head urls

    unpack body dir = do
        when (verbosity > Verbosity.Silent) do
            hPutStr stdout
                ( "  Unpacking '" <> sourceName <> "' to " <> targetDir
                <> "… "
                )
            hFlush stdout
        r <- try do
            Zip.extractFilesFromArchive [Zip.OptDestination dir]
                (Zip.toArchive body)

            removePathForcibly targetDir
            renameDirectory dir targetDir
        case r of
            Left e -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn stdout "failed"
                when (verbosity >= Verbosity.Annoying) do
                    hPutStrLn stderr
                        ( "ERROR: Unpacking of " <> url
                        <> " failed with: "
                        <> displayException @SomeException e
                        )

            Right _ -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn stdout "success"

pageCandidatePaths
    :: FilePath
    -> [String]
    -> SomePlatform
    -> [Locale]
    -> String
    -> [FilePath]
pageCandidatePaths cacheDir sources platform locales command =
    [ cacheDir </> s </> ("pages" <> l) </> p </> command <.> "md"
    | s <- sources
    , p <- platforms
    , l <- languages
    ]
  where
    languages :: [String]
    languages = flip concatMap locales \case
        l@Locale{language = LanguageCode.EN, country = Nothing} ->
            -- Language "en" is the default and is not represented in the path,
            -- but this is idiosyncratic and custom pages may want to use for
            -- example `pages.en` instead of `pages`.
            [renderLocale l, ""]

        l@Locale{language = LanguageCode.EN, country = Just _} ->
            -- Same as above, but in case of locales like "en_GB" we want to
            -- try that first before defaulting to "en".
            [renderLocale l, renderLocale l{country = Nothing}, ""]

        l@Locale{country = Just _} ->
            [renderLocale l, renderLocale l{country = Nothing}]

        l ->
            [renderLocale l]

    renderLocale :: Locale -> String
    renderLocale = ("." <>) . Text.unpack . localeToText

    platforms :: [String]
    platforms = case platform of
        AllPlatforms ->
            ["android", "linux", "macos", "osx", "sunos", "windows", "common"]
        KnownPlatform p -> case p of
            Android -> ["android", "common"]
            Linux -> ["linux", "common"]
            Osx -> ["osx", "common"]
            Sunos -> ["sunos", "common"]
            Windows -> ["windows", "common"]
        OtherPlatform p ->
            [p, "common"]

currentPlatform :: SomePlatform
currentPlatform = case CI.mk os of
    "darwin" -> KnownPlatform Osx
    "linux" -> KnownPlatform Linux
    "linux-android" -> KnownPlatform Android
    "mingw32" -> KnownPlatform Windows
    _ -> OtherPlatform (Char.toLower <$> os)
