-- |
-- Module:      TldrClient.Client
-- Description: Tldr pages client logic
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Tldr pages client logic.
module TldrClient.Client
    ( client

    -- * Client action
    , Action(..)
    , RenderParams(..)
    , RenderFileParams(..)
    , ListParams(..)
    , UpdateParams(..)
    , ClearCeacheParams(..)

    -- * Client standard input and output
    , InputOutput(..)
    )
  where

import Control.Applicative (pure)
import Control.Exception
    ( Exception(displayException)
    , SomeException
    , throwIO
    , try
    )
import Control.Monad (when)
import Data.Bool (Bool(False, True), (&&), not, )
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (/=))
import Data.Foldable (and, concat, for_, length, null)
import Data.Function (($), (.))
import Data.Functor ((<$), (<$>), (<&>), )
import Data.List qualified as List (elem, filter, notElem)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, listToMaybe)
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.String (String, fromString, unlines)
import Data.Traversable (for)
import System.Exit (exitFailure)
import System.IO
    ( FilePath
    , Handle
    , IO
    , hClose
    , hFlush
    , hPutStr
    , hPutStrLn
    )
import Text.Show (Show, show)

import Codec.Archive.Zip qualified as Zip
    ( ZipOption(OptDestination)
    , extractFilesFromArchive
    , toArchive
    )
import Control.Lens (view)
import Data.ByteString qualified as ByteString (hGetContents, hPutStr, )
import Data.ByteString.Lazy qualified as Lazy (ByteString, )
--import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import Data.Text qualified as Text (intercalate, unpack)
import Data.Verbosity qualified as Verbosity (Verbosity(Silent))
import Database.SQLite.Simple qualified as SQLite (withConnection)
import Network.Wreq qualified as Wreq (get, responseBody, )
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removePathForcibly
    , renameDirectory
    )
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withTempDirectory, withTempFile)
import Tldr qualified (renderPage, )
import Tldr.Types qualified as Tldr (ColorSetting({-NoColor,-} UseColor), )

import Data.List.Compat (List, )
import TldrClient.Configuration
    ( Configuration(sources, verbosity, prefixes)
    , Source(format, location, name)
    , SourceLocation(Local, Remote)
    , SourceFormat(TldrPagesWithIndex, TldrPagesWithoutIndex)
    , getCacheDirectory
    , getLocales
    , putDebugLn
    , putErrorLn
    , putWarningLn
    )
import TldrClient.Index qualified as Index
    ( Entry(..)
    , ListQuery(..)
    , LookupQuery(..)
    , PruneQuery(..)
    , list
    , load
    , lookup
    , newUnlessExists
    , prune
    )
import TldrClient.Locale (Locale(..), )
import TldrClient.Locale qualified as Locale (toText, )
import TldrClient.Platform (SomePlatform(..), )
import TldrClient.Platform qualified as Platform
import TldrClient.TldrPagesIndex qualified as TldrPagesIndex
    ( indexAndLoad
    , load
    )


data Action
    = Render RenderParams
    -- ^ Render a tldr page for a specific command and other attributes, see
    -- `RenderParams` for details.
    | RenderFile RenderFileParams
    -- ^ Render a specific tldr page file.
    | List ListParams
    -- ^ List tldr pages based on the filter, see `ListParams` for details.
    | Update UpdateParams
    -- ^ Update local cache
    | ClearCache ClearCeacheParams
  deriving stock (Eq, Show)

data RenderParams = RenderParams
    { platformOverride :: Maybe (SomePlatform Text)
    , localeOverride :: Maybe Locale
    , sourcesOverride :: List Text
    , command :: String
    -- ^ Command to list tldr page for.
    }
  deriving stock (Eq, Show)

newtype RenderFileParams = RenderFileParams
    { input :: Either Handle FilePath
    }
  deriving stock (Eq, Show)

data ListParams = ListParams
    { platformOverride :: Maybe (SomePlatform Text)
    , localeOverride :: Maybe Locale
    , sourcesOverride :: List Text
    }
  deriving stock (Eq, Show)

newtype UpdateParams = UpdateParams
    { sourcesOverride :: List Text
    }
  deriving stock (Eq, Show)

data ClearCeacheParams = ClearCeacheParams
    { platformOverride :: Maybe (SomePlatform Text)
    , localeOverride :: Maybe Locale
    , sourcesOverride :: List Text
    }
  deriving stock (Eq, Show)

data InputOutput = InputOutput
    { errorOutput :: Handle
    , standardOutput :: Handle
    }
  deriving stock (Eq, Show)

client :: Configuration -> InputOutput -> Action -> IO ()
client config inputOutput action = do
    putDebugLn config errorOutput ("Configuration: " <> show config)
    putDebugLn config errorOutput ("Action: " <> show action)

    cacheDirectory <- getCacheDirectory config
    putDebugLn config errorOutput ("Cache directory: " <> cacheDirectory)

    createDirectoryIfMissing True cacheDirectory
    indexFile <- Index.newUnlessExists cacheDirectory
    putDebugLn config errorOutput ("Index file: " <> indexFile)

    case action of
        Render RenderParams{..} -> do
            locales <- getLocales config errorOutput localeOverride
            entries <- SQLite.withConnection indexFile \connection -> do
                let doLookup prefix = do
                        let query = Index.LookupQuery
                                { command = prefix <> fromString command
                                , sources =
                                    -- TODO: The override source may not exist
                                    -- (be configured) and we should check it
                                    -- to git better error message.
                                    nonEmpty sourcesOverride
                                , locales = Just (Locale.toText <$> locales)
                                , platforms = getPlatforms platformOverride
                                }
                        putDebugLn config errorOutput ("Query: " <> show query)
                        Index.lookup connection query

                if null config.prefixes
                    then
                        doLookup ""
                    else
                        concat <$> for config.prefixes \prefix ->
                            doLookup (prefix <> "-")

            putDebugLn config errorOutput "Entries that were found:"
            for_ entries \entry ->
                putDebugLn config errorOutput ("  " <> show entry)

            case listToMaybe entries of
                Nothing -> do
                    missingPageMessage config inputOutput command
                    exitFailure

                Just entry ->
                    renderEntry config inputOutput cacheDirectory entry

        RenderFile RenderFileParams{..} -> do
            let withInput :: (FilePath -> IO ()) -> IO ()
                withInput f =
                    case input of
                        Left h₀ -> do
                            content <- ByteString.hGetContents h₀
                            withTempFile cacheDirectory "page.md" \file h₁ -> do
                                ByteString.hPutStr h₁ content
                                hClose h₁
                                f file

                        Right file ->
                            f file

            withInput (renderPage config inputOutput)

        List ListParams{..} -> do
            locales <- getLocales config errorOutput localeOverride
            entries <- SQLite.withConnection indexFile \connection -> do
                let doList prefix = do
                        let query = Index.ListQuery
                                { sources =
                                    -- TODO: The override source may not exist
                                    -- (be configured) and we should check it
                                    -- to git better error message.
                                    nonEmpty sourcesOverride
                                , locales = Just (Locale.toText <$> locales)
                                , platforms = getPlatforms platformOverride
                                , prefix
                                }
                        putDebugLn config errorOutput ("Query: " <> show query)
                        Index.list connection query

                if null config.prefixes
                    then
                        doList ""
                    else
                        concat <$> for config.prefixes \prefix ->
                            doList (prefix <> "-")

            for_ entries \entry -> do
                let Index.Entry
                        { source
                        , locale
                        , platform
                        , command
                        , filePath
                        } = entry

                hPutStrLn standardOutput case filePath of
                    Just path -> Text.unpack source <> ": " <> path
                    Nothing ->
                        let path = "pages." <> Text.unpack locale
                                </> Text.unpack platform
                                </> Text.unpack command <.> "md"
                                <> " (cache only)"
                        in  Text.unpack source <> ": " <> path

        Update UpdateParams{..} -> do
            let sourcesToFetch :: List Source
                sourcesToFetch = if null sourcesOverride
                    then NonEmpty.toList config.sources
                    else List.filter
                            (\s -> s.name `List.elem` sourcesOverride)
                            (NonEmpty.toList config.sources)

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

            when haveSpecifiedUnknownSource do
                let sourceNames :: List Text
                    sourceNames = sourcesToFetch <&> (.name)

                    unknownSources :: String
                    unknownSources =
                        Text.unpack . Text.intercalate ", "
                        $ List.filter (`List.notElem` sourceNames)
                            sourcesOverride

                putWarningLn config errorOutput
                    ( "Unknown page sources specified on command line: "
                    <> unknownSources
                    )

            -- Since configuration contains non-empty list this can happen only
            -- if only unknown sources were specified on the command line.
            when (null sourcesToFetch) do
                let sourceNames :: String
                    sourceNames =
                        Text.unpack . Text.intercalate ", "
                        $ NonEmpty.toList config.sources <&> (.name)

                putErrorLn config errorOutput
                    ( "No known (configured) page source was specified on the\
                    \ command line. Configured sources are: " <> sourceNames
                    )

                exitFailure

            -- TODO: Purge sources that do not exist in configuration anymore.

            updateResults <- for sourcesToFetch \source -> do
                when (config.verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput
                        ("Updating '" <> Text.unpack source.name <> "'…")
                updateWasSuccessful <- updateCache inputOutput UpdateCacheParams
                    { config
                    , cacheDirectory
                    , indexFile
                    , source
                    }
                when (config.verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput
                        if updateWasSuccessful
                            then "… done."
                            else "… failed."
                pure updateWasSuccessful

            let thereWasAnUpdateError = not (and updateResults)
            when thereWasAnUpdateError do
                putErrorLn config errorOutput
                    "One or more sources failed to update; exit(1)."
                exitFailure

        ClearCache ClearCeacheParams{..} -> do
            locales <- getLocales config errorOutput localeOverride
            let query = Index.PruneQuery
                    { sources =
                        -- TODO: The override source may not exist (be
                        -- configured) and we should check it to git better
                        -- error message.
                        nonEmpty sourcesOverride
                    , locales = Just (Locale.toText <$> locales)
                    , platforms = getPlatforms platformOverride
                    }
            putDebugLn config errorOutput ("Query: " <> show query)
            SQLite.withConnection indexFile \connection ->
                Index.prune connection query
  where
    InputOutput{errorOutput, standardOutput} = inputOutput

data FailedToLoadTldrPagesIndex = FailedToLoadTldrPagesIndex
    { source :: String
    , reason :: String
    }
  deriving stock (Show)

instance Exception FailedToLoadTldrPagesIndex where
    displayException :: FailedToLoadTldrPagesIndex -> String
    displayException FailedToLoadTldrPagesIndex{..} =
        "Failed to load tldr-pages index ('index.json') for source '"
        <> source <> "': " <> reason

data UpdateCacheParams = UpdateCacheParams
    { config :: Configuration
    , cacheDirectory :: FilePath
    , indexFile :: FilePath
    , source :: Source
    }

-- | Update offline cache and index. Return value indicates if the process was
-- successful or not:
--
-- * `False` — Failed to update offline cache, usually because we failed to
--   download the remote file or unpack it.
--
-- * `True` — Successfully updated offline cache.
updateCache :: InputOutput -> UpdateCacheParams -> IO Bool
updateCache InputOutput{errorOutput, standardOutput} UpdateCacheParams{..} =
    case source.location of
        Local dir -> do
            when (config.verbosity > Verbosity.Silent) do
                hPutStr standardOutput ("  Indexing '" <> dir <> "'… ")
                hFlush standardOutput

            r <- try do
                -- TODO: Detect if the target is an archive and unpack it
                -- first.
                indexSource
                    IndexSourceParams
                        { indexFile
                        , source = source.name
                        , format = source.format
                        , dir
                        }
            case r :: Either SomeException () of
                Left _ -> False <$ do
                    when (config.verbosity > Verbosity.Silent) do
                        hPutStrLn standardOutput "failed"

                Right () -> True <$ do
                    when (config.verbosity > Verbosity.Silent) do
                        hPutStrLn standardOutput "success"

        Remote urls -> do
            let sourceName :: String
                sourceName = Text.unpack source.name

                targetDir :: FilePath
                targetDir = cacheDirectory </> sourceName

            putDebugLn config errorOutput
                ("Target directory for '" <> sourceName <> "': " <> targetDir)

            withTempDirectory cacheDirectory sourceName \dir ->
                downloadThenUnpackAndIndex sourceName targetDir dir
                    (NonEmpty.toList urls)
  where
    downloadThenUnpackAndIndex
        :: String
        -> FilePath
        -> FilePath
        -> List String
        -> IO Bool
    downloadThenUnpackAndIndex sourceName targetDir dir = loop
      where
        loop = \case
            [] -> pure False
            url : mirrors -> do
                when (config.verbosity > Verbosity.Silent) do
                    hPutStr standardOutput
                        ( "  Downloading '" <> sourceName
                        <> "' (" <> url <> ")… "
                        )
                    hFlush standardOutput

                httpResponse <- try (Wreq.get url)
                case httpResponse of
                    Left e -> do
                        when (config.verbosity > Verbosity.Silent) do
                            hPutStrLn standardOutput "failed"
                        putErrorLn config errorOutput
                            ( "Download of " <> url <> " failed with: "
                            <> displayException @SomeException e
                            )

                        loop mirrors

                    Right (view Wreq.responseBody -> body) -> do
                        when (config.verbosity > Verbosity.Silent) do
                            hPutStrLn standardOutput "success"

                        unpackAndIndex sourceName targetDir url dir body

    unpackAndIndex
        :: String
        -> FilePath
        -> String
        -> FilePath
        -> Lazy.ByteString
        -> IO Bool
    unpackAndIndex sourceName targetDir url dir body = do
        when (config.verbosity > Verbosity.Silent) do
            hPutStr standardOutput
                ( "  Unpacking and indexing '" <> sourceName
                <> "' (may take a while)… "
                )
            hFlush standardOutput
        r <- try do
            Zip.extractFilesFromArchive [Zip.OptDestination dir]
                (Zip.toArchive body)

            indexSource IndexSourceParams
                { indexFile
                , source = source.name
                , format = source.format
                , dir
                }

            removePathForcibly targetDir
            -- TODO: This takes a lot of space. Maybe we should just not keep
            -- it around?
            renameDirectory dir targetDir
        case r of
            Left e -> False <$ do
                when (config.verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput "failed"
                putErrorLn config errorOutput
                    ( "Unpacking of " <> url <> " failed with: "
                    <> displayException @SomeException e
                    )

            Right _ -> True <$ do
                when (config.verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput "success"

-- | Get list of platforms to be searched when looking up a page.
getPlatforms
    :: Maybe (SomePlatform Text)
    -- ^ Override current platform.
    --
    -- * `Nothing` — use current platform, see `Platfrom.current` for more
    --   information.
    --
    -- * @`Just` p@ — use specified platform @p@ instead of current platform.
    -> Maybe (NonEmpty Text)
    -- ^ Platform filter for when we are looking up a page.
    --
    -- * `Nothing` — we are not filtering by platform at all, i.e. all pages
    --   for all platforms will be searched when looking up a page.
    --
    -- * @`Just` ps@ — only platfroms listed in @ps@ will be used when
    --   searching for a page.
getPlatforms platformOverride =
    case fromMaybe Platform.current platformOverride of
        AllPlatforms ->
            Nothing

        -- Be aware that the \"tldr pages\" project is planning to move from
        -- @osx@ to @macos@ some time in the future. For that reason we are now
        -- supporting both with preference for @osx@.
        KnownPlatform p -> Just case p of
            Platform.Android -> "android" :| ["common"]
            Platform.Freebsd -> "freebsd" :| ["common"]
            Platform.Linux   -> "linux"   :| ["common"]
            Platform.Macos   -> "osx"     :| ["macos", "common"]
            Platform.Netbsd  -> "netbsd"  :| ["common"]
            Platform.Openbsd -> "openbsd" :| ["common"]
            Platform.Sunos   -> "sunos"   :| ["common"]
            Platform.Windows -> "windows" :| ["common"]

        OtherPlatform p ->
            Just (p :| ["common"])

renderEntry :: Configuration -> InputOutput -> FilePath -> Index.Entry -> IO ()
renderEntry config inputOutput cacheDirectory Index.Entry{..} =
    case filePath of
        Nothing ->
            renderContent

        Just relativePath -> do
            let path = cacheDirectory </> Text.unpack source
                    </> relativePath
            fileExists <- doesFileExist path
            if fileExists
                then renderFile path
                else do
                    putDebugLn config inputOutput.errorOutput
                        (path <> ": File not found, using cached content.")
                    renderContent
  where
    renderContent = do
        putDebugLn config inputOutput.errorOutput "Page found in cache only."

        let file = Text.unpack command <.> "md"
        withTempFile cacheDirectory file \path h -> do
            ByteString.hPutStr h content
            hClose h
            renderPage config inputOutput path

    renderFile path = do
        putDebugLn config inputOutput.errorOutput ("Page found: " <> path)
        renderPage config inputOutput path

renderPage :: Configuration -> InputOutput -> FilePath -> IO ()
renderPage _config InputOutput{standardOutput} file =
    Tldr.renderPage file standardOutput Tldr.UseColor

data IndexSourceParams = IndexSourceParams
    { indexFile :: FilePath
    , source :: Text
    , format :: SourceFormat
    , dir :: FilePath
    }

indexSource :: IndexSourceParams -> IO ()
indexSource IndexSourceParams{..} =
    SQLite.withConnection indexFile \connection -> do
        Index.prune connection Index.PruneQuery
            { sources = Just (pure source)
            , locales = Nothing
            , platforms = Nothing
            }

        let loadBatch = Index.load connection
        case format of
            TldrPagesWithIndex -> do
                TldrPagesIndex.load source dir loadBatch \reason ->
                    throwIO FailedToLoadTldrPagesIndex
                        { source = Text.unpack source
                        , reason
                        }

            TldrPagesWithoutIndex ->
                TldrPagesIndex.indexAndLoad source dir loadBatch

missingPageMessage :: Configuration -> InputOutput -> String -> IO ()
missingPageMessage config inputOutput command =
    when (config.verbosity > Verbosity.Silent) do
        hPutStr inputOutput.standardOutput $ unlines
            [ "Unable to find page for command '" <> command <> "'"
            , ""
            , "Possible reasons why this may have had happened:"
            , ""
            , "* There is a typo in the '" <> command <> "'. If you are unsure\
                \ how command is spelled then try:"
            , ""
            , "    tldr --list"
            , ""
            , "* Offline cache is outdated. To fix this try running:"
            , ""
            , "    tldr --update"
            , ""
            , "* Page is really missing for that command. You can propose it's\
                \ addition to the official tldr-pages set by going to:"
            , ""
            , "    " <> requestPageUrl
            , ""
            , "  Or if its something that is not meant to be public you can\
              \ add it into a into a local set of tldr-pages. See `tldr(1)`\
              \ manual page for more information."
            ]
  where
    requestPageUrl :: String
    requestPageUrl =
        "https://github.com/tldr-pages/tldr/issues/new?title=page%20request:%20"
        <> urlEncode command

    urlEncode :: String -> String
    urlEncode t = t -- TODO
