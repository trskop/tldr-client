-- |
-- Module:      TldrClient.Locale
-- Description: Represents locale (language & region)
-- Copyright:   (c) 2021-2024 Peter Trško
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
    , parse
    , toText
    , defaultLocale

    -- * Dhall Encoding
    , decode

    -- * Locale Environment Variables
    , parseLocaleEnv
    , languagePreference
    )
  where

import Control.Applicative (pure, )
import Control.Monad.Fail (fail, )
import Control.Monad ((>>=), unless, )
import Data.Bool ((&&), otherwise, )
import Data.Either (Either(Left, Right), either, )
import Data.Eq (Eq, (/=), (==), )
import Data.Foldable (elem, )
import Data.Function ((.), )
import Data.Functor ((<$>), (<&>), )
import Data.List.NonEmpty (NonEmpty((:|)), (<|), )
import Data.List.NonEmpty qualified as NonEmpty (nub, prependList, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Semigroup ((<>), )
import Data.String (String, )
import Data.Traversable (for, )
import GHC.Generics (Generic, )
import Text.Show (Show, show, )

import Control.Monad.Except (throwError, )
import Data.Aeson (FromJSON(..), ToJSON(..), )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson (Parser, )
import Data.CountryCodes (CountryCode(..), )
import Data.CountryCodes qualified as CountryCode (decode, fromText, toString, )
import Data.LanguageCodes.Extended (LanguageCode, )
import Data.LanguageCodes.Extended qualified as LanguageCode
import Data.Text (Text)
import Data.Text qualified as Text
    ( break
    , drop
    , length
    , null
    , splitAt
    , takeWhile
    , uncons
    , )
import Dhall qualified (Decoder, field, maybe, record, )
import System.Environment.Parser
    ( ParseEnv
    , ParseEnvError(ParseEnvError)
    , optionalVar
    , )

-- | Locale format:
--
-- > <language>[_<country>]
data Locale = Locale
    { language :: LanguageCode
    , country :: Maybe CountryCode
    }
  deriving stock (Eq, Generic, Show)

decode :: Dhall.Decoder Locale
decode = Dhall.record do
    language <- Dhall.field "language" LanguageCode.decode
    country <- Dhall.field "country" (Dhall.maybe CountryCode.decode)
    pure Locale{..}

instance FromJSON Locale where
    parseJSON :: Aeson.Value -> Aeson.Parser Locale
    parseJSON = Aeson.withText "Locale" (either fail pure . parse)

instance ToJSON Locale where
    toJSON :: Locale -> Aeson.Value
    toJSON = toJSON . toText

    toEncoding :: Locale -> Aeson.Encoding
    toEncoding = toEncoding . toText

toText :: Locale -> Text
toText Locale{..} =
    LanguageCode.toString language
    <> maybe "" (\c -> "_" <> CountryCode.toString c) country

-- | Parse text value in @ll[_CC]@ format into a `Locale`.
parse :: Text -> Either String Locale
parse t = do
    -- Split `LL[_CC]` into a `LL` and optionally `CC` portion.
    (languageText, possiblyCountryText) <- do
        let -- Few examples of what this does:
            --
            -- "en"        → ("en","")
            -- "en_GB"     → ("en","_GB")
            -- ""          → ("", "")
            -- "x"         → ("x", "")
            -- "enInvalid" → ("en", "Invalid")
            (lang, rest) = Text.splitAt 2 t
        unless (Text.length lang == 2) do
            failUnableToParseLocale t
        (lang, ) <$> case Text.uncons rest of
            -- Country code is optional:
            Nothing ->
                pure Nothing
            -- '_' must be right before country code:
            Just ('_', c) | Text.length c == 2 ->
                pure (Just c)
            _ ->
                failUnableToParseLocale t

    language <- maybe (failUnknownLanguage languageText) pure do
        LanguageCode.fromText languageText

    country <- for possiblyCountryText \c ->
        maybe (failUnknownCountry c) pure do
            CountryCode.fromText c

    pure Locale{..}
  where
    failUnableToParseLocale :: Text -> Either String a
    failUnableToParseLocale x = Left
        ( "Unable to parse as Locale: expected format <language>[_<country>],\
        \ but got the following value: " <> show x
        )

    failUnknownLanguage :: Text -> Either String a
    failUnknownLanguage l =
        Left ("Unknown language code in Locale: " <> show l)

    failUnknownCountry :: Text -> Either String a
    failUnknownCountry c = Left ("Unknow country code in Locale: " <> show c)

-- | Parse locale (e.g. @LANG@) environment variable value that is in the
-- following format:
--
-- > [language[_territory][.codeset][@modifier]]
--
-- Special values that are also supported:
--
-- > C[.codeset][@modifier]
--
-- ### See also
--
-- * [Locale (computer software)
--   ](https://en.wikipedia.org/wiki/Locale_%28computer_software%29)
-- * [ArchWiki: Locale](https://wiki.archlinux.org/title/locale)
parseLocaleEnv :: Text -> Either String (Maybe Locale)
parseLocaleEnv s
  | Text.null s =
        -- Setting LANG or LANGUAGE to empty string is the same thing as
        -- keeping it unset.
        pure Nothing

  | s' == "C" =
        -- @\"C\"@ is a special value that means no localisation. In our case
        -- this is the same thing as `defaultLocale`.
        pure (Just defaultLocale)

  | otherwise =
        Just <$> parse s'
  where
    s' = Text.takeWhile (\c -> c /= '.' && c /= '@') s

-- | Figure out user language preference from system environment.
--
-- The algorithm itself is described in [Tldr-pages client specification v2.2:
-- Language](https://github.com/tldr-pages/tldr/blob/v2.2/CLIENT-SPECIFICATION.md#language)
--
-- ### See also
--
-- Additional resources regarding Locale Environment Variables:
--
-- * [wiki.debian.org/Locale](https://wiki.debian.org/Locale)
-- * [GNU gettext utilities: 2.3 Setting the Locale through Environment
--   Variables](https://www.gnu.org/savannah-checkouts/gnu/gettext/manual/html_node/Setting-the-POSIX-Locale.html)
languagePreference :: ParseEnv context (NonEmpty Locale)
languagePreference = do
    -- Relevant parts of "tldr-pages client specification v2.2 section
    -- Language" (step 4. is not relevant):
    --
    -- 1. Check the value of LANG. If not set, then skip to step 5.
    -- 2. Extract the priority list from LANGUAGE. If not set,
    --    start with an empty priority list.
    -- 3. Append the value of LANG to the priority list.
    -- 4. Follow the priority list in order and use the first available
    --    language.
    -- 5. Fall back to English if none of the languages are
    --    available.
    let fallbackList :: NonEmpty Locale
        fallbackList = pure defaultLocale
    langs <- lookupLang >>= \case
        -- This is the jump from step 1. directly to step 5. when LANG is empty.
        Nothing ->
            pure fallbackList

        -- Based on step 3. and 5. we need to append the value of LANG and the
        -- fallback language.
        Just lang -> do
            lookupLanguage <&> (`NonEmpty.prependList` (lang <| fallbackList))

    let -- This is a deviation from tldr-pages client specification v2.2. Here
        -- we assume that people from different regions will be able to read
        -- more general language. Reason for doing this is the fact that the
        -- specification defines that the language code used for pages
        -- translation should be the shortest one. So if there are translated
        -- pages for example for czech language then `cs` code should be used
        -- for pages translations instead of `cs_CZ`, however, `LANG`
        -- environment variable will be using `cs_CZ`.
        modifiedLangs = langs >>= \case
            l@Locale{country = Just _}
              -- Checking presence not only simplifies the list, but also helps
              -- preserve user preferences more closely.
              | l{country = Nothing} `elem` langs ->
                    pure @NonEmpty l
              | otherwise ->
                    l :| [l{country = Nothing}]
            l ->
                pure @NonEmpty l

    -- The step 3. and 5. as described in "tldr-pages client specification v2.2
    -- section Language", see above, are not completely clear. Based on the
    -- wording it seems like the specification mandates to always append LANG
    -- (when available) and English (defaultLocale) to the list, however, that
    -- doesn't make much sense if it is already in the priority list. The
    -- example that is listed in the aformentioned section confirms this
    -- assumption for the LANG value. We do the same for the whole list
    -- including defaultLocale (English), ufortunately we don't have a way to
    -- confirm this assumption since the examples there do not cover this case.
    pure (NonEmpty.nub modifiedLangs)

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
            case parseLocaleEnv x of
                Left err -> throwError (ParseEnvError "LANGUAGE" err)
                Right l -> do
                    ls <- splitLanguage (Text.drop 1 rest)
                    pure (maybe ls (: ls) l)

lookupLang :: forall context. ParseEnv context (Maybe Locale)
lookupLang = optionalVar "LANG" >>= \case
    Nothing -> pure Nothing
    Just t ->
        case parseLocaleEnv t of
            Left err -> throwError (ParseEnvError "LANG" err)
            Right l -> pure l

-- | [Tldr-pages client specification v2.2: Language
-- ](https://github.com/tldr-pages/tldr/blob/v2.2/CLIENT-SPECIFICATION.md#language)
-- defines fallback locale as __English__.
defaultLocale :: Locale
defaultLocale = Locale
    { language = LanguageCode.EN
    , country = Nothing
    }
