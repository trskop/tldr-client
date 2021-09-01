-- |
-- Module:      TldrClient.Locale
-- Description: Represents locale (language & region)
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Represents locale (language & region).
module TldrClient.Locale
    (
    -- * Index
      Locale(..)
    , LanguageCode
    , decodeLocale
    , parseLocale
    , localeToText
    , defaultLocale
    , languagePreference
    )
  where

import Prelude (minBound, maxBound)

import Control.Applicative (pure)
import Control.Monad.Fail (fail)
import Control.Monad ((>>=))
import Data.Bool ((&&), otherwise)
import Data.Char (Char)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (elem)
import Data.Function ((.))
import Data.Functor ((<$), (<$>))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Text.Show (Show, show)

import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.CountryCodes (CountryCode(..))
import qualified Data.CountryCodes as CountryCode
import Data.LanguageCodes (ISO639_1)
import qualified Data.LanguageCodes as LanguageCode
import Data.Text (Text)
import qualified Data.Text as Text
    ( break
    , cons
    , drop
    , null
    , singleton
    , takeWhile
    , toUpper
    , uncons
    )
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
import System.Environment.Parser
    ( ParseEnv
    , ParseEnvError(ParseEnvError)
    , optionalVar
    )

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

-- | Figure out user language preference from system environment.
--
-- The algorithm itself is described in [Tldr-pages client specification v1.5:
-- Language](https://github.com/tldr-pages/tldr/blob/v1.5/CLIENT-SPECIFICATION.md#language)
--
-- Additional resources regarding Locale Environment Variables:
--
-- * [wiki.debian.org/Locale](https://wiki.debian.org/Locale)
-- * [GNU gettext utilities: 2.3 Setting the Locale through Environment
--   Variables](https://www.gnu.org/savannah-checkouts/gnu/gettext/manual/html_node/Setting-the-POSIX-Locale.html)
languagePreference :: ParseEnv context (NonEmpty Locale)
languagePreference = do
    possiblyLang <- lookupLang
    r <- for possiblyLang \lang -> do
        priorityList <- lookupLanguage
        -- This is a small deviation from tldr-pages client specification
        -- v1.5 where it mandates to always append lang to the list,
        -- however, that doesn't make much sense if it is already in the
        -- priorityList.
        pure
            if (lang `elem` priorityList)
                then priorityList
                else priorityList <> [lang]

    let langs :: NonEmpty Locale
        langs = fromMaybe (pure @NonEmpty defaultLocale) (r >>= nonEmpty)

    pure do
        -- This is a deviation from tldr-pages client specification v1.5. Here
        -- we assume that people from different regions will be able to read
        -- more general language. Reason for doing this is the fact that the
        -- specification defines that the language code used for pages
        -- translation should be the shortest one. So if there are translated
        -- pages for example for czech language then `cs` code should be used
        -- for pages translations instead of `cs_CZ`, however, `LANG`
        -- environment variable will be using `cs_CZ`.
        langs >>= \case
            l@Locale{country = Just _}
              -- Checking presence not only simplifies the list, but also helps
              -- preserve user preferences more closely.
              | l{country = Nothing} `elem` langs ->
                    pure @NonEmpty l
              | otherwise ->
                    l :| [l{country = Nothing}]
            l ->
                pure @NonEmpty l

-- | Lookup `LANGUAGE` environment variable. If it is not set or set to empty
-- value then this function will return @[]@ (empty list).
lookupLanguage :: forall context. ParseEnv context [Locale]
lookupLanguage = optionalVar "LANGUAGE" >>= maybe (pure []) splitLanguage
  where
    splitLanguage :: Text -> ParseEnv context [Locale]
    splitLanguage t
      | Text.null t = pure []
      | otherwise = do
            let (x, rest) = Text.break (== ':') t
            l <- parseLocale' x
            ls <- splitLanguage (Text.drop 1 rest)
            pure (maybe ls (: ls) l)

    parseLocale' :: Text -> ParseEnv context (Maybe Locale)
    parseLocale' s
      | Text.null s =
            -- Setting LANGUAGE to empty string is the same thing as keeping it
            -- unset.
            pure Nothing

      | s == "C" =
            -- @\"C\"@ is a special value that means no localisation. In our
            -- case this is the same thing as `defaultLocale`.
            pure (Just defaultLocale)

      | otherwise =
            case parseLocale (Text.takeWhile (\c -> c /= '.' && c /= '@') s) of
                Right r -> pure (Just r)
                Left err -> throwError (ParseEnvError "LANGUAGE" err)

lookupLang :: ParseEnv context (Maybe Locale)
lookupLang = do
    rawLang <- do
        l <- optionalVar "LANG"
        -- Setting LANG to empty string is the same thing as keeping it unset.
        pure if
          | Just t <- l, Text.null t -> Nothing
          | otherwise -> l

    for rawLang \(t :: Text) -> if
      | t == "C" ->
            -- @\"C\"@ is a special value that means no localisation. In our
            -- case this is the same thing as `defaultLocale`.
            pure defaultLocale

      | otherwise ->
          case parseLocale (Text.takeWhile (\c -> c /= '.' && c /= '@') t) of
                Left err -> throwError (ParseEnvError "LANG" err)
                Right lang -> pure lang

defaultLocale :: Locale
defaultLocale = Locale
    { language = LanguageCode.EN
    , country = Nothing
    }
