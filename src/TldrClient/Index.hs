-- |
-- Module:      TldrClient.Index
-- Description: Local cache in the form of SQLite database
-- Copyright:   (c) 2021-2023 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Local cache in the form of SQLite database.
module TldrClient.Index
    ( Entry(..)

    -- * Create
    , new
    , newUnlessExists
    , load
    , getIndexFile

    -- * Lookup
    , LookupQuery(..)
    , lookup

    -- * List
    , ListQuery(..)
    , list

    -- * Prune
    , PruneQuery(..)
    , prune

    -- * Completion
    , getLocales
    , getPlatforms
    , getCommands
    )
  where

import Control.Applicative ((<*>), pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Eq (Eq)
import Data.Foldable (concat, for_)
import Data.Functor ((<$), (<$>))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup ((<>))
import Data.Traversable (for)
import System.IO (FilePath)
import Text.Show (Show)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.SQLite.Simple.FromRow qualified as SQLite (RowParser)
import Database.SQLite.Simple.ToField qualified as SQLite (toField)
import Database.SQLite.Simple qualified as SQLite
    ( Connection
    , FromRow(fromRow)
    , Only(Only)
    , SQLData
    , ToRow(toRow)
    , executeMany
    , execute
    , execute_
    , query
    , withConnection
    )
import System.Directory (doesFileExist)
import System.FilePath ((</>))


data Entry = Entry
    { source :: Text
    , command :: Text
    , platform :: Text
    , locale :: Text
    , content :: ByteString
    , filePath :: Maybe FilePath
    }
  deriving stock (Eq, Show)

instance SQLite.FromRow Entry where
    fromRow :: SQLite.RowParser Entry
    fromRow = do
        (source, command, platform, locale, content, filePath) <- SQLite.fromRow
        pure Entry{..}

instance SQLite.ToRow Entry where
    toRow :: Entry -> [SQLite.SQLData]
    toRow Entry{..} =
        [ SQLite.toField source
        , SQLite.toField command
        , SQLite.toField platform
        , SQLite.toField locale
        , SQLite.toField content
        , SQLite.toField filePath
        ]

init :: MonadIO m => SQLite.Connection -> m ()
init connection = liftIO do
    SQLite.execute_ connection
        "CREATE TABLE IF NOT EXISTS pages_index\n\
        \  ( sequenceId INTEGER PRIMARY KEY\n\
        \  , source TEXT NOT NULL\n\
        \  , command TEXT NOT NULL\n\
        \  , platform TEXT NOT NULL\n\
        \  , locale TEXT NOT NULL\n\
        \  , content TEXT NOT NULL\n\
        \  , file_path TEXT NULL\n\
        \  );"

indexFile :: FilePath -> FilePath
indexFile cacheDirectory = cacheDirectory </> "index.sqlite"

getIndexFile :: MonadIO m => FilePath -> m (Maybe FilePath)
getIndexFile cacheDirectory = liftIO do
    let file = indexFile cacheDirectory
    fileExists <- doesFileExist file
    pure if fileExists
        then Just file
        else Nothing

load :: MonadIO m => SQLite.Connection -> [Entry] -> m ()
load connection entries = liftIO do
    SQLite.executeMany connection
        "INSERT INTO\
        \ pages_index (source, command, platform, locale, content, file_path)\
        \ VALUES (?, ?, ?, ?, ?, ?)"
        entries

new :: MonadIO m => FilePath -> m FilePath
new cacheDirectory = liftIO do
    let dbFile = indexFile cacheDirectory
    dbFile <$ SQLite.withConnection dbFile init

newUnlessExists :: MonadIO m => FilePath -> m FilePath
newUnlessExists cacheDirectory = liftIO do
    possiblyFile <- getIndexFile cacheDirectory
    maybe (new cacheDirectory) pure possiblyFile

data LookupQuery = LookupQuery
    { command :: Text
    , sources :: Maybe (NonEmpty Text)
    , platforms :: Maybe (NonEmpty Text)
    , locales :: Maybe (NonEmpty Text)
    }
  deriving stock (Eq, Show)

-- TODO: This implementation is very naive, maybe there's a much better way of
-- doing this.
lookup :: MonadIO m => SQLite.Connection -> LookupQuery -> m [Entry]
lookup connection LookupQuery{..} = liftIO if
  | Nothing <- sources, Nothing <- platforms, Nothing <- locales ->
        SQLite.query connection
            "SELECT source, command, platform, locale, content, file_path\
            \ FROM pages_index\
            \ WHERE command = ?"
            (SQLite.Only command)

  | Just ss <- sources, Nothing <- platforms, Nothing <- locales ->
        concat <$> for ss \source ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command = ?\
                \   AND source = ?"
                (command, source)

  | Nothing <- sources, Just ps <- platforms, Nothing <- locales ->
        concat <$> for ps \platform ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command = ?\
                \   AND platform = ?"
                (command, platform)

  | Nothing <- sources, Nothing <- platforms, Just ls <- locales ->
        concat <$> for ls \locale ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command = ?\
                \   AND locale = ?"
                (command, locale)

  | Nothing <- sources, Just ps <- platforms, Just ls <- locales -> do
        concat <$> for ((,) <$> ps <*> ls) \(platform, locale) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command = ?\
                \   AND platform = ?\
                \   AND locale = ?"
                (command, platform, locale)

  | Just ss <- sources, Nothing <- platforms, Just ls <- locales -> do
        concat <$> for ((,) <$> ss <*> ls) \(source, locale) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command = ?\
                \   AND source = ?\
                \   AND locale = ?"
                (command, source, locale)

  | Just ss <- sources, Just ps <- platforms, Nothing <- locales -> do
        concat <$> for ((,) <$> ss <*> ps) \(source, platform) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command = ?\
                \   AND source = ?\
                \   AND platform = ?"
                (command, source, platform)

  | Just ss <- sources, Just ps <- platforms, Just ls <- locales -> do
        let params = (,,) <$> ss <*> ps <*> ls
        concat <$> for params \(source, platform, locale) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command = ?\
                \   AND source = ?\
                \   AND platform = ?\
                \   AND locale = ?"
                (command, source, platform, locale)

data ListQuery = ListQuery
    { sources :: Maybe (NonEmpty Text)
    , platforms :: Maybe (NonEmpty Text)
    , locales :: Maybe (NonEmpty Text)
    , prefix :: Text
    }
  deriving stock (Eq, Show)

-- TODO: This implementation is very naive, maybe there's a much better way of
-- doing this.
list :: MonadIO m => SQLite.Connection -> ListQuery -> m [Entry]
list connection ListQuery{..} = liftIO if
  | Nothing <- sources, Nothing <- platforms, Nothing <- locales ->
        SQLite.query connection
            "SELECT source, command, platform, locale, content, file_path\
            \ FROM pages_index\
            \ WHERE command LIKE ?"
            (SQLite.Only prefix)

  | Just ss <- sources, Nothing <- platforms, Nothing <- locales ->
        concat <$> for ss \source ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command LIKE ? \
                \   AND source = ?"
                (prefix <> "%", source)

  | Nothing <- sources, Just ps <- platforms, Nothing <- locales ->
        concat <$> for ps \platform ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command LIKE ? \
                \   AND platform = ?"
                (prefix <> "%", platform)

  | Nothing <- sources, Nothing <- platforms, Just ls <- locales ->
        concat <$> for ls \locale ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command LIKE ?\
                \   AND locale = ?"
                (prefix <> "%", locale)

  | Nothing <- sources, Just ps <- platforms, Just ls <- locales -> do
        concat <$> for ((,) <$> ps <*> ls) \(platform, locale) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command LIKE ?\
                \   AND platform = ?\
                \   AND locale = ?"
                (prefix <> "%", platform, locale)

  | Just ss <- sources, Nothing <- platforms, Just ls <- locales -> do
        concat <$> for ((,) <$> ss <*> ls) \(source, locale) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command LIKE ?\
                \   AND source = ?\
                \   AND locale = ?"
                (prefix <> "%", source, locale)

  | Just ss <- sources, Just ps <- platforms, Nothing <- locales -> do
        concat <$> for ((,) <$> ss <*> ps) \(source, platform) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command LIKE ?\
                \   AND source = ?\
                \   AND platform = ?"
                (prefix <> "%", source, platform)

  | Just ss <- sources, Just ps <- platforms, Just ls <- locales -> do
        let params = (,,) <$> ss <*> ps <*> ls
        concat <$> for params \(source, platform, locale) ->
            SQLite.query connection
                "SELECT source, command, platform, locale, content, file_path\
                \ FROM pages_index\
                \ WHERE command LIKE ?\
                \   AND source = ?\
                \   AND platform = ?\
                \   AND locale = ?"
                (prefix <> "%", source, platform, locale)

data PruneQuery
    = PruneQuery
        { sources :: Maybe (NonEmpty Text)
        , locales :: Maybe (NonEmpty Text)
        , platforms :: Maybe (NonEmpty Text)
        }
  deriving stock (Eq, Show)

prune :: MonadIO m => SQLite.Connection -> PruneQuery -> m ()
prune connection PruneQuery{..} = liftIO if
  | Nothing <- sources
  , Nothing <- locales
  , Nothing <- platforms ->
        SQLite.execute_ connection "DELETE FROM pages_index"

  | Nothing <- sources
  , Nothing <- locales
  , Just platforms' <- platforms ->
        for_ platforms' \platform ->
            SQLite.execute connection
                "DELETE FROM pages_index WHERE platform = ?"
                (SQLite.Only platform)

  | Nothing <- sources
  , Just locales' <- locales
  , Nothing <- platforms ->
        for_ locales' \locale ->
            SQLite.execute connection
                "DELETE FROM pages_index WHERE locale = ?"
                (SQLite.Only locale)

  | Nothing <- sources
  , Just locales' <- locales
  , Just platforms' <- platforms ->
        for_ ((,) <$> locales' <*> platforms') \(locale, platform) ->
            SQLite.execute connection
                "DELETE FROM pages_index WHERE locale = ? AND platform = ?"
                (locale, platform)

  | Just sources' <- sources
  , Nothing <- locales
  , Nothing <- platforms ->
        for_ sources' \source ->
            SQLite.execute connection
                "DELETE FROM pages_index WHERE source = ?"
                (SQLite.Only source)

  | Just sources' <- sources
  , Nothing <- locales
  , Just platforms' <- platforms ->
        for_ ((,) <$> sources' <*> platforms') \(source, platform) ->
            SQLite.execute connection
                "DELETE FROM pages_index WHERE source = ? AND platform = ?"
                (source, platform)

  | Just sources' <- sources
  , Just locales' <- locales
  , Nothing <- platforms ->
        for_ ((,) <$> sources' <*> locales') \(source, locale) ->
            SQLite.execute connection
                "DELETE FROM pages_index WHERE source = ? AND locale = ?"
                (source, locale)

  | Just sources' <- sources
  , Just locales' <- locales
  , Just platforms' <- platforms -> do
        let params = (,,) <$> sources' <*> locales' <*> platforms'
        for_ params \(source, locale, platform) ->
            SQLite.execute connection
                "DELETE FROM pages_index WHERE\
                \ source = ? AND locale = ? AND platform = ?"
                (source, locale, platform)

getLocales :: MonadIO m => SQLite.Connection -> Text -> m [Text]
getLocales connection prefix = coerce @[SQLite.Only Text] <$> liftIO do
    SQLite.query connection
        "SELECT DISTINCT locale FROM pages_index WHERE locale LIKE ?"
        (SQLite.Only (prefix <> "%"))

getPlatforms :: MonadIO m => SQLite.Connection -> Text -> m [Text]
getPlatforms connection prefix = coerce @[SQLite.Only Text] <$> liftIO do
    SQLite.query connection
        "SELECT DISTINCT platform FROM pages_index WHERE platform LIKE ?"
        (SQLite.Only (prefix <> "%"))

getCommands :: MonadIO m => SQLite.Connection -> Text -> m [Text]
getCommands connection prefix = coerce @[SQLite.Only Text] <$> liftIO do
    SQLite.query connection
        "SELECT DISTINCT command FROM pages_index WHERE command LIKE ?"
        (SQLite.Only (prefix <> "%"))
