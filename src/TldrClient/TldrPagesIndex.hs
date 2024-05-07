-- |
-- Module:      TldrClient.TldrPagesIndex
-- Description: Structure and indexing of tldr-pages
-- Copyright:   (c) 2021-2023 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Structure and indexing of tldr-pages, this includes:
--
-- * @index.json@ file present in official distribution of tldr-pages.
--
-- * Indexing of tldr-pages directory structure without @index.json@.
module TldrClient.TldrPagesIndex
    (
    -- * Index
      Index(..)
    , Command(..)
    , Target(..)
    , load
    , indexAndLoad
    )
  where

import Control.Applicative ((<|>), pure)
import Control.Monad ((>>=))
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (==))
import Data.Foldable (for_, null)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.List qualified as List (drop, filter, isPrefixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Traversable (for)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Show (Show)

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson (Parser)
import Data.Aeson.Encoding qualified as Aeson (text)
import Data.ByteString qualified as ByteString (readFile)
import Data.LanguageCodes qualified as LanguageCode (ISO639_1(EN))
import Data.Text (Text)
import Data.Text qualified as Text (null, unpack)
import System.Directory (listDirectory)
import System.FilePath ((<.>), (</>), dropExtension, takeExtension)

import TldrClient.Locale (Locale(..), )
import TldrClient.Locale qualified as Locale (toText, )
import TldrClient.Index qualified as Index (Entry(..))


-- | Represents @index.json@ of @tldr-pages@.
data Index = Index
    { commands :: [Command]
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Command = Command
    { name :: Text
    , platform :: NonEmpty Text
    , language :: NonEmpty SomeLocale
    , targets :: NonEmpty Target
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Target = Target
    { os :: Text
    , language :: SomeLocale
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SomeLocale
    = KnownLocale Locale
    | UnknownLocale Text
  deriving stock (Eq, Generic, Show)

someLocaleToText :: SomeLocale -> Text
someLocaleToText = \case
    KnownLocale l -> Locale.toText l
    UnknownLocale t -> t

instance FromJSON SomeLocale where
    parseJSON :: Aeson.Value -> Aeson.Parser SomeLocale
    parseJSON = Aeson.withText "SomeLocale" \t ->
        (KnownLocale <$> parseJSON (Aeson.String t)) <|> pure (UnknownLocale t)

instance ToJSON SomeLocale where
    toJSON :: SomeLocale -> Aeson.Value
    toJSON = \case
        KnownLocale l -> toJSON l
        UnknownLocale t -> Aeson.String t

    toEncoding :: SomeLocale -> Aeson.Encoding
    toEncoding = \case
        KnownLocale l -> toEncoding l
        UnknownLocale t -> Aeson.text t

load
    :: Text
    -- ^ Source name.
    -> FilePath
    -- ^ Directory where we expect @index.json@.
    -> ([Index.Entry] -> IO ())
    -- ^ Process a batch of entries.
    -> (String -> IO ())
    -- ^ On JSON parsing error.
    -> IO ()
load source dir process onJsonParsingError = do
    let indexFile = dir </> "index.json"
    Aeson.eitherDecodeFileStrict' indexFile >>= \case
        Left err ->
            onJsonParsingError err
        Right Index{commands} ->
            for_ commands \command -> do
                entries <- for (commandToEntries command) readContent
                process entries
  where
    commandToEntries :: Command -> [Index.Entry]
    commandToEntries Command{name, targets} = NonEmpty.toList do
        Target{..} <- targets
        let locale = someLocaleToText language
        pure Index.Entry
            { source
            , command = name
            , platform = os
            , locale
            , content = "" -- Empty content, will need to be populated.
            , filePath = Just
                -- pages${locale}/${platform}/${command}.md
                ( pagesDir language locale </> Text.unpack os
                </> Text.unpack name <.> "md"
                )
            }

    readContent :: Index.Entry -> IO Index.Entry
    readContent entry@Index.Entry{filePath} = case filePath of
        Nothing ->
            -- This should never happen due to how entries are
            -- constructed above.
            pure entry
        Just file -> do
            content <- ByteString.readFile (dir </> file)
            pure entry{Index.content}

    pagesDir :: SomeLocale -> Text -> String
    pagesDir language t = "pages" <> case language of
        KnownLocale Locale{language = LanguageCode.EN, country = Nothing} -> ""
        _ | Text.null t -> ""
          | otherwise   -> "." <> Text.unpack t

-- |
--
-- > ${root}/
-- >
indexAndLoad
    :: Text
    -- ^ Source name.
    -> FilePath
    -- ^ Directory with `pages${lang}` directories that doesn't have
    -- @index.json@.
    -> ([Index.Entry] -> IO ())
    -- ^ Process a batch of entries.
    -> IO ()
indexAndLoad source dir loadBatch = do
    -- TODO: Check that the directory structure matches and warn if it doesn't.
    -- For example, level1 should be non-empty.
    level1 <- List.filter ("pages" `List.isPrefixOf`) <$> listDirectory dir
    for_ level1 \d1 -> do
        platforms <- listDirectory (dir </> d1)
        for_ platforms \platform -> do
            let d = dir </> d1 </> platform
            fs <- List.filter ((".md" ==) . takeExtension) <$> listDirectory d
            entries <- for fs \f -> do
                content <- ByteString.readFile (d </> f)
                pure Index.Entry
                    { source
                    , command = fromString (dropExtension f)
                    , platform = fromString platform
                    , locale = fromString (getLocale d1)
                    , content
                    , filePath = Just (d1 </> platform </> f)
                    }
            loadBatch entries
  where
    getLocale :: String -> String
    getLocale d =
        let lang = List.drop 1 (takeExtension d)
        in  if null lang
                then "en"
                else fromString lang
