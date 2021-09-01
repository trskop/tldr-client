-- |
-- Module:      Client
-- Description: Tldr pages client logic
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Tldr pages client logic.
module Client
    ( client
    , Action(..)
    , SomePlatform(..)
    , Platform(..)
    , parsePlatform
    )
  where

import Control.Applicative ((*>), pure)
import Control.Exception
    ( Exception(displayException)
    , SomeException
    , onException
    , throwIO
    , try
    )
import Control.Monad (when)
import Data.Bool (Bool(True), (&&), not)
import qualified Data.Char as Char (toLower)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (/=))
import Data.Foldable (for_, length, null)
import Data.Function (($), (.))
import Data.Functor ((<$>), (<&>))
import qualified Data.List as List (elem, filter, intercalate, notElem)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (head, toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, listToMaybe)
import Data.Ord ((>), (>=))
import Data.Semigroup ((<>))
import Data.String (String, fromString, unlines)
import System.Exit (exitFailure)
import System.IO
    ( FilePath
    , IO
    , hClose
    , hFlush
    , hPrint
    , hPutStr
    , hPutStrLn
    , stderr
    , stdout
    )
import System.Info (os)
import Text.Show (Show, show)

import qualified Codec.Archive.Zip as Zip
    ( ZipOption(OptDestination)
    , extractFilesFromArchive
    , toArchive
    )
import Control.Lens (view)
import qualified Data.ByteString as ByteString (hPutStr)
import qualified Data.CaseInsensitive as CI (mk)
--import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import qualified Data.Text as Text (intercalate, unpack)
import qualified Data.Verbosity as Verbosity (Verbosity(Annoying, Silent))
import qualified Database.SQLite.Simple as SQLite (withConnection)
import Network.Wreq (get, responseBody)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removePathForcibly
    , renameDirectory
    )
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withTempDirectory, withTempFile)
import qualified Tldr (renderPage)
import qualified Tldr.Types as Tldr (ColorSetting({-NoColor,-} UseColor))

import Configuration
    ( Configuration(Configuration, sources, verbosity)
    , Source(Source, format, location, name)
    , SourceLocation(Local, Remote)
    , SourceFormat(TldrPagesWithIndex, TldrPagesWithoutIndex)
    , getCacheDirectory
    , getLocales
    )
import qualified TldrClient.Index as Index
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
import qualified TldrClient.TldrPagesIndex as TldrPagesIndex
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

client :: Configuration -> Action -> IO ()
client config@Configuration{sources, verbosity} action = do
    when (verbosity >= Verbosity.Annoying) do
        -- TODO: Find a nicer way to print these:
        hPutStr stderr "DEBUG: " *> hPrint stderr config
        hPutStr stderr "DEBUG: " *> hPrint stderr action

    cacheDirectory <- getCacheDirectory config
    when (verbosity >= Verbosity.Annoying) do
        hPutStrLn stderr ("DEBUG: Cache directory: " <> cacheDirectory)

    createDirectoryIfMissing True cacheDirectory
    indexFile <- Index.newUnlessExists cacheDirectory

    case action of
        Render platformOverride localeOverride sourcesOverride commands -> do
            let command = List.intercalate "-" commands
            locales <- getLocales config localeOverride
            entries <- SQLite.withConnection indexFile \connection -> do
                let query = Index.LookupQuery
                        { command = fromString command
                        , sources =
                            -- TODO: The override source may not exist (be
                            -- configured) and we should check it to git better
                            -- error message.
                            nonEmpty sourcesOverride
                        , locales = Just (localeToText <$> locales)
                        , platforms = getPlatforms platformOverride
                        }
                when (verbosity >= Verbosity.Annoying) do
                    hPutStrLn stderr ("DEBUG: Query: " <> show query)
                Index.lookup connection query

            when (verbosity >= Verbosity.Annoying) do
                hPutStrLn stderr "DEBUG: Entries that were found:"
                for_ entries \entry ->
                    hPutStrLn stderr ("  " <> show entry)

            case listToMaybe entries of
                Nothing -> do
                    missingPageMessage config command
                    exitFailure

                Just entry ->
                    renderEntry config cacheDirectory entry

        List platformOverride localeOverride sourcesOverride -> do
            locales <- getLocales config localeOverride
            entries <- SQLite.withConnection indexFile \connection -> do
                let query = Index.ListQuery
                        { sources =
                            -- TODO: The override source may not exist (be
                            -- configured) and we should check it to git better
                            -- error message.
                            nonEmpty sourcesOverride
                        , locales = Just (localeToText <$> locales)
                        , platforms = getPlatforms platformOverride
                        }
                when (verbosity >= Verbosity.Annoying) do
                    hPutStrLn stderr ("DEBUG: Query: " <> show query)
                Index.list connection query

            for_ entries \entry -> do
                let Index.Entry
                        { source
                        , locale
                        , platform
                        , command
                        , filePath
                        } = entry

                hPutStrLn stdout case filePath of
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

            -- TODO: Purge sources that do not exist in configuration anymore.

            for_ sourcesToFetch \source@Source{name} -> do
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn stdout ("Updating '" <> Text.unpack name <> "'…")
                updateCache UpdateCacheParams
                  { config
                  , cacheDirectory
                  , indexFile
                  , source
                  }
                when (verbosity > Verbosity.Silent) do
                    hPutStrLn stdout ("… done.")

        ClearCache platformOverride localeOverride sourcesOverride -> do
            locales <- getLocales config localeOverride
            let query = Index.PruneQuery
                    { sources =
                        -- TODO: The override source may not exist (be
                        -- configured) and we should check it to git better
                        -- error message.
                        nonEmpty sourcesOverride
                    , locales = Just (localeToText <$> locales)
                    , platforms = getPlatforms platformOverride
                    }
            when (verbosity >= Verbosity.Annoying) do
                hPutStrLn stderr ("DEBUG: Query: " <> show query)
            SQLite.withConnection indexFile \connection ->
                Index.prune connection query

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

updateCache :: UpdateCacheParams -> IO ()

updateCache
  UpdateCacheParams
    { config = Configuration{verbosity}
    , indexFile
    , source = Source{name, format, location = Local dir}
    } = do
    when (verbosity > Verbosity.Silent) do
        hPutStr stdout ("  Indexing '" <> dir <> "'… ")
        hFlush stdout

    indexSource IndexSourceParams{indexFile, source = name, format, dir}
        `onException` when (verbosity > Verbosity.Silent) do
            hPutStrLn stdout "failed"

    when (verbosity > Verbosity.Silent) do
        hPutStrLn stdout "success"

updateCache
  UpdateCacheParams
    { config = Configuration{verbosity}
    , cacheDirectory
    , indexFile
    , source = Source{name, format, location = Remote urls}
    } =
  do
    when (verbosity >= Verbosity.Annoying) do
        hPutStrLn stderr
            ( "DEBUG: Target directory for '" <> sourceName <> "': "
            <> targetDir
            )

    withTempDirectory cacheDirectory sourceName \dir -> do
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
            hPutStr stdout
                ( "  Unpacking and indexing '" <> sourceName
                <> "' (may take a while)… "
                )
            hFlush stdout
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

renderEntry :: Configuration -> FilePath -> Index.Entry -> IO ()
renderEntry Configuration{verbosity} cacheDirectory Index.Entry{..} =
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
                    when (verbosity >= Verbosity.Annoying) do
                        hPutStrLn stderr
                            ( "DEBUG: "
                            <> path
                            <> ": File not found, using cached content."
                            )
                    renderContent
  where
    renderContent = do
        when (verbosity >= Verbosity.Annoying) do
            hPutStrLn stderr ("DEBUG: Page found in cache only.")

        let file = Text.unpack command <.> "md"
        withTempFile cacheDirectory file \path h -> do
            ByteString.hPutStr h content
            hClose h
            Tldr.renderPage path stdout Tldr.UseColor

    renderFile path = do
        when (verbosity >= Verbosity.Annoying) do
            hPutStrLn stderr ("DEBUG: Page found: " <> path)

        Tldr.renderPage path stdout Tldr.UseColor

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

missingPageMessage :: Configuration -> String -> IO ()
missingPageMessage Configuration{verbosity} command =
    when (verbosity > Verbosity.Silent) do
        hPutStr stdout $ unlines
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
            ]
  where
    requestPageUrl =
        "https://github.com/tldr-pages/tldr/issues/new?title=page%20request:%20"
        <> urlEncode command

    urlEncode t = t -- TODO
