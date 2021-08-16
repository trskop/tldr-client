-- |
-- Module:      Locale
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Locale
    (
    -- * Index
      Locale(..)
    , LanguageCode
    , lookupLang
    , decodeLocale
    , parseLocale
    , localeToText
    )
  where

import Prelude (minBound, maxBound)

import Control.Applicative (pure)
import Control.Monad.Fail (fail)
import Control.Monad (when)
import Data.Bool (otherwise)
import Data.Char (Char)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq, (/=))
import Data.Function (($), (.))
import Data.Functor ((<$), (<$>))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO (IO, putStrLn)
import Text.Show (Show, show)

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.CountryCodes (CountryCode(..))
import qualified Data.CountryCodes as CountryCode
import Data.LanguageCodes (ISO639_1)
import qualified Data.LanguageCodes as LanguageCode
import Data.Text (Text)
import qualified Data.Text as Text
    ( cons
    , null
    , singleton
    , takeWhile
    , toUpper
    , uncons
    )
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import qualified Dhall
    ( Decoder(Decoder, expected, extract)
    , field
    , maybe
    , record
    , typeError
    )
import qualified Dhall.Core as Dhall (Expr(Field, Union), fieldSelectionLabel)
import qualified Dhall.Map as Dhall (Map)
import qualified Dhall.Map (fromList, lookup)

-- | Locale format:
--
-- > <language>[_<country>]
data Locale = Locale
    { language :: LanguageCode
    , country :: Maybe CountryCode
    }
  deriving stock (Eq, Generic, Show)

decodeLocale :: Dhall.Decoder Locale
decodeLocale = Dhall.record do
    language <- Dhall.field "language" decodeLanguageCode
    country <- Dhall.field "country" (Dhall.maybe decodeCountryCode)
    pure Locale{..}

decodeLanguageCode :: Dhall.Decoder LanguageCode
decodeLanguageCode = enum (Dhall.Map.fromList values)
  where
    toText :: LanguageCode -> Text
    toText l =
        let (l1, l2) = LanguageCode.toChars l
        in  Text.toUpper (Text.cons l1 (Text.singleton l2))

    values :: [(Text, LanguageCode)]
    values =
        -- ISO639_1 doesn't have `Bounded` instance.
        [(toText l, l) | l <- [LanguageCode.AA .. LanguageCode.ZU]]

decodeCountryCode :: Dhall.Decoder CountryCode
decodeCountryCode = enum (Dhall.Map.fromList values)
  where
    values :: [(Text, CountryCode)]
    values = [(CountryCode.toText c, c) | c <- [minBound .. maxBound]]

enum :: Dhall.Map Text a -> Dhall.Decoder a
enum values = Dhall.Decoder
    { extract = \expr -> case expr of
        Dhall.Field (Dhall.Union _) (Dhall.fieldSelectionLabel -> field) ->
            maybe (Dhall.typeError (pure unionType) expr) pure
                (Dhall.Map.lookup field values)
        _ ->
            Dhall.typeError (pure unionType) expr

    , expected = pure unionType
    }
  where
    unionType :: forall s a. Dhall.Expr s a
    unionType = Dhall.Union (Nothing <$ values)

type LanguageCode = ISO639_1

instance FromJSON Locale where
    parseJSON :: Aeson.Value -> Aeson.Parser Locale
    parseJSON = Aeson.withText "Locale" \t ->
        either fail pure (parseLocale t)

instance ToJSON Locale where
    toJSON :: Locale -> Aeson.Value
    toJSON = toJSON . localeToText

    toEncoding :: Locale -> Aeson.Encoding
    toEncoding = toEncoding . localeToText

localeToText :: Locale -> Text
localeToText Locale{..} =
    Text.cons l1 (Text.singleton l2)
    <> maybe "" (\c -> "_" <> CountryCode.toText c) country
  where
    (l1, l2) = LanguageCode.toChars language

parseLocale :: Text -> Either String Locale
parseLocale t = do
    (language, possiblyCountry) <- do
        -- Parsing separately characters for language (format LL[_CC]).
        -- This is due to how `LanguageCode.fromChars` works.
        (l1, l2, c) <- maybe (failUnableToParseLocale t) pure do
            (l1, r1) <- Text.uncons t
            (l2, r2) <- Text.uncons r1
            pure (l1, l2, r2)
        (, c) <$> maybe (failUnknownLanguage l1 l2) pure do
            LanguageCode.fromChars l1 l2

    country <- if
        -- Country code is optional:
      | Text.null possiblyCountry ->
          pure Nothing
        -- '_' must be right before country code:
      | Just ('_', c) <- Text.uncons possiblyCountry ->
          case CountryCode.fromMText c of
              Nothing ->
                  failUnknownCountry c
              r ->
                  pure r
        -- Unknown format:
      | otherwise ->
          failUnableToParseLocale t

    pure Locale{..}
  where
    failUnableToParseLocale :: Text -> Either String a
    failUnableToParseLocale x = Left
        ( "Unable to parse as Locale: expected format <language>[_<country>],\
        \ but got: " <> show x
        )

    failUnknownLanguage :: Char -> Char -> Either String a
    failUnknownLanguage l1 l2 =
        Left ("Unknown language code in Locale: " <> show [l1, l2])

    failUnknownCountry :: Text -> Either String a
    failUnknownCountry c = Left ("Unknow country code in Locale: " <> show c)

lookupLang :: Verbosity -> Locale -> IO Locale
lookupLang verbosity def = do
    lang <- lookupEnv "LANG"
    maybe (pure def) (parseLang . fromString) lang
  where
    parseLang :: Text -> IO Locale
    parseLang t = case parseLocale (Text.takeWhile (/= '.') t) of
        Right locale ->
            pure locale
        Left err ->
            def <$ when (verbosity >= Verbosity.Normal) do
                putStrLn
                    $ "Warning: LANG: Failed to parse environment variable,\
                    \ defaulting to " <> show (localeToText def) <>
                    "; parsing error: " <> err
