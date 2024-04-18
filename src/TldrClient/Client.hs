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
    , Action(..)
    , SomePlatform(..)
    , Platform(..)
    , InputOutput(..)
    , parsePlatform
    )
  where

import Control.Applicative (pure)
import Control.Exception
    ( Exception(displayException)
    , SomeException
    , onException
    , throwIO
    , try
    )
import Control.Monad (when)
import Data.Bool (Bool(True), (&&), not)
import Data.Char qualified as Char (toLower)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (/=))
import Data.Foldable (concat, for_, length, null)
import Data.Function (($), (.))
import Data.Functor ((<$>), (<&>))
import Data.List qualified as List (elem, filter, intercalate, notElem)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, toList)
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
import System.Info (os)
import Text.Show (Show, show)

import Codec.Archive.Zip qualified as Zip
    ( ZipOption(OptDestination)
    , extractFilesFromArchive
    , toArchive
    )
import Control.Lens (view)
import Data.ByteString qualified as ByteString (hPutStr)
import Data.CaseInsensitive qualified as CI (mk)
--import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import Data.Text qualified as Text (intercalate, unpack)
import Data.Verbosity qualified as Verbosity (Verbosity(Silent))
import Database.SQLite.Simple qualified as SQLite (withConnection)
import Network.Wreq (get, responseBody)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removePathForcibly
    , renameDirectory
    )
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withTempDirectory, withTempFile)
import Tldr qualified (renderPage)
import Tldr.Types qualified as Tldr (ColorSetting({-NoColor,-} UseColor))

import TldrClient.Configuration
    ( Configuration(Configuration, sources, verbosity, prefixes)
    , Source(Source, format, location, name)
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
import TldrClient.Locale (Locale(..), localeToText)
import TldrClient.TldrPagesIndex qualified as TldrPagesIndex
    ( indexAndLoad
    , load
    )


data Action
    = Render (Maybe SomePlatform) (Maybe Locale) [Text] [String]
    | List (Maybe SomePlatform) (Maybe Locale) [Text]
    | Update [Text]
    | ClearCache (Maybe SomePlatform) (Maybe Locale) [Text]
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

data InputOutput = InputOutput
    { errorOutput :: Handle
    , standardOutput :: Handle
    }

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
        Render platformOverride localeOverride sourcesOverride commands -> do
            let command = List.intercalate "-" commands
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
                                , locales = Just (localeToText <$> locales)
                                , platforms = getPlatforms platformOverride
                                }
                        putDebugLn config errorOutput ("Query: " <> show query)
                        Index.lookup connection query

                if null prefixes
                    then
                        doLookup ""
                    else
                        concat <$> for prefixes \prefix ->
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

        List platformOverride localeOverride sourcesOverride -> do
            locales <- getLocales config errorOutput localeOverride
            entries <- SQLite.withConnection indexFile \connection -> do
                let doList prefix = do
                        let query = Index.ListQuery
                                { sources =
                                    -- TODO: The override source may not exist
                                    -- (be configured) and we should check it
                                    -- to git better error message.
                                    nonEmpty sourcesOverride
                                , locales = Just (localeToText <$> locales)
                                , platforms = getPlatforms platformOverride
                                , prefix
                                }
                        putDebugLn config errorOutput ("Query: " <> show query)
                        Index.list connection query

                if null prefixes
                    then
                        doList ""
                    else
                        concat <$> for prefixes \prefix ->
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

            when haveSpecifiedUnknownSource do
                let sourceNames :: [Text]
                    sourceNames = sourcesToFetch <&> \Source{name} -> name

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
                        $ NonEmpty.toList sources <&> \Source{name} -> name

                putErrorLn config errorOutput
                    ( "No known (configured) page source was specified on the\
                    \ command line. Configured sources are: " <> sourceNames
                    )

                exitFailure

            -- TODO: Purge sources that do not exist in configuration anymore.

            for_ sourcesToFetch \source@Source{name} -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput
                        ("Updating '" <> Text.unpack name <> "'…")
                updateCache inputOutput UpdateCacheParams
                  { config
                  , cacheDirectory
                  , indexFile
                  , source
                  }
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput "… done."

        ClearCache platformOverride localeOverride sourcesOverride -> do
            locales <- getLocales config errorOutput localeOverride
            let query = Index.PruneQuery
                    { sources =
                        -- TODO: The override source may not exist (be
                        -- configured) and we should check it to git better
                        -- error message.
                        nonEmpty sourcesOverride
                    , locales = Just (localeToText <$> locales)
                    , platforms = getPlatforms platformOverride
                    }
            putDebugLn config errorOutput ("Query: " <> show query)
            SQLite.withConnection indexFile \connection ->
                Index.prune connection query
  where
    Configuration{sources, verbosity, prefixes} = config
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

updateCache :: InputOutput -> UpdateCacheParams -> IO ()

updateCache
  InputOutput{standardOutput}
  UpdateCacheParams
    { config = Configuration{verbosity}
    , indexFile
    , source = Source{name, format, location = Local dir}
    } = do
    when (verbosity > Verbosity.Silent) do
        hPutStr standardOutput ("  Indexing '" <> dir <> "'… ")
        hFlush standardOutput

    indexSource IndexSourceParams{indexFile, source = name, format, dir}
        `onException` when (verbosity > Verbosity.Silent) do
            hPutStrLn standardOutput "failed"

    when (verbosity > Verbosity.Silent) do
        hPutStrLn standardOutput "success"

updateCache
  InputOutput{errorOutput, standardOutput}
  UpdateCacheParams
    { config = config@Configuration{verbosity}
    , cacheDirectory
    , indexFile
    , source = Source{name, format, location = Remote urls}
    } =
  do
    putDebugLn config errorOutput
        ("Target directory for '" <> sourceName <> "': " <> targetDir)

    withTempDirectory cacheDirectory sourceName \dir -> do
        when (verbosity > Verbosity.Silent) do
            hPutStr standardOutput
                ("  Downloading '" <> sourceName <> "' (" <> url <> ")… ")
            hFlush standardOutput

        -- TODO: At the moment we only try the primary URL, however, we should
        -- try the mirrors if we fail to download the primary URL.
        httpResponse <- try (get url)
        case httpResponse of
            Left e -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput "failed"
                putErrorLn config errorOutput
                    ( "Download of " <> url <> " failed with: "
                    <> displayException @SomeException e
                    )

            Right (view responseBody -> body) -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput "success"

                unpackAndIndex body dir
  where
    sourceName :: String
    sourceName = Text.unpack name

    targetDir :: FilePath
    targetDir = cacheDirectory </> sourceName

    url :: String
    url = NonEmpty.head urls

    unpackAndIndex body dir = do
        when (verbosity > Verbosity.Silent) do
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
                , source = name
                , format
                , dir
                }

            removePathForcibly targetDir
            -- TODO: This takes a lot of space. Maybe we should just not keep
            -- it around?
            renameDirectory dir targetDir
        case r of
            Left e -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput "failed"
                putErrorLn config errorOutput
                    ( "Unpacking of " <> url <> " failed with: "
                    <> displayException @SomeException e
                    )

            Right _ -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn standardOutput "success"

currentPlatform :: SomePlatform
currentPlatform = case CI.mk os of
    "darwin" -> KnownPlatform Osx
    "linux" -> KnownPlatform Linux
    "linux-android" -> KnownPlatform Android
    "mingw32" -> KnownPlatform Windows
    _ -> OtherPlatform (Char.toLower <$> os)

getPlatforms :: Maybe SomePlatform -> Maybe (NonEmpty Text)
getPlatforms platformOverride =
    case fromMaybe currentPlatform platformOverride of
        AllPlatforms ->
            Nothing

        KnownPlatform p -> Just case p of
            Android -> "android" :| ["common"]
            Linux -> "linux" :| ["common"]
            Osx -> "osx" :| ["common"]
            Sunos -> "sunos" :| ["common"]
            Windows -> "windows" :| ["common"]

        OtherPlatform p ->
            Just (fromString p :| ["common"])

renderEntry :: Configuration -> InputOutput -> FilePath -> Index.Entry -> IO ()
renderEntry config InputOutput{..} cacheDirectory Index.Entry{..} =
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
                    putDebugLn config errorOutput
                        (path <> ": File not found, using cached content.")
                    renderContent
  where
    renderContent = do
        putDebugLn config errorOutput "Page found in cache only."

        let file = Text.unpack command <.> "md"
        withTempFile cacheDirectory file \path h -> do
            ByteString.hPutStr h content
            hClose h
            Tldr.renderPage path standardOutput Tldr.UseColor

    renderFile path = do
        putDebugLn config errorOutput ("Page found: " <> path)

        Tldr.renderPage path standardOutput Tldr.UseColor

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
    when (verbosity > Verbosity.Silent) do
        hPutStr standardOutput $ unlines
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
    Configuration{verbosity} = config
    InputOutput{standardOutput} = inputOutput

    requestPageUrl :: String
    requestPageUrl =
        "https://github.com/tldr-pages/tldr/issues/new?title=page%20request:%20"
        <> urlEncode command

    urlEncode :: String -> String
    urlEncode t = t -- TODO
