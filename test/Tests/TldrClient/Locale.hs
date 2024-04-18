-- |
-- Module:      Tests.TldrClient.Locale
-- Description: Unit tests for the module TldrClient.Locale
-- Copyright:   (c) 2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Unit tests for the module "TldrClient.Locale".
module Tests.TldrClient.Locale
    ( tests
    , )
  where

import Control.Applicative (pure, )
import Data.Bifunctor (first, )
import Data.Either (Either(Left, Right), )
import Data.List.NonEmpty (NonEmpty((:|)), )
import Data.Function (($), (&), (.), id, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Semigroup ((<>), )
import Text.Show (Show, show, )
import System.IO (IO, )

import Data.CallStack (HasCallStack, )
import Data.HashMap.Strict (HashMap, )
import Data.HashMap.Strict qualified as HashMap (empty, insert, )
import Data.Text (Text, )
import System.Environment.Parser (parseEnv, )

import Test.Tasty (TestTree, testGroup, )
import Test.Tasty.HUnit ((@?=), assertFailure, testCase, )

import Data.CountryCodes qualified as CountryCode
import Data.LanguageCodes.Extended qualified as LanguageCode
import TldrClient.Locale


tests :: [TestTree]
tests =
    [ testCase "parseLocale" do
        parseLocale "en" @?= Right en
        parseLocale "en_GB" @?= Right en_GB
        expectParseFailure (parseLocale "")
        expectParseFailure (parseLocale "e")
        expectParseFailure (parseLocale "en_")
        expectParseFailure (parseLocale "xx")
        expectParseFailure (parseLocale "xx_XX")

    , testCase "parseLocaleEnv" do
        parseLocaleEnv "" @?= Right Nothing
        parseLocaleEnv "C" @?= Right (Just defaultLocale)
        parseLocaleEnv "en" @?= Right (Just en)
        parseLocaleEnv "en_GB" @?= Right (Just en_GB)
        parseLocaleEnv "en_GB.utf8" @?= Right (Just en_GB)
        parseLocaleEnv "en_GB.UTF-8" @?= Right (Just en_GB)
        parseLocaleEnv "de_AT.ISO8859-15" @?= Right (Just de_AT)
        expectParseFailure (parseLocale "e")
        expectParseFailure (parseLocale "en_")
        expectParseFailure (parseLocale "xx")
        expectParseFailure (parseLocale "xx_XX")

    , testCase "localeToText" do
        localeToText en @?= "en"
        localeToText en_GB @?= "en_GB"

    , test_languagePreference
    ]

test_languagePreference :: TestTree
test_languagePreference = testGroup "languagePreference"
    -- This is based on the example from [tldr-pages client
    -- specification 2.2: Language
    -- ](https://github.com/tldr-pages/tldr/blob/main/CLIENT-SPECIFICATION.md#language),
    -- however, there is a difference:
    --
    -- "cz" was changed to "cs" as "cz" is not actually a valid
    -- language code, it's a country code.
    [ testCase "tldr-pages client specification 2.2 example" do
        test (Just "cs") (Just "it:cs:de") (it :| [cs, de, en])
        test (Just "cs") (Just "it:de:fr") (it :| [de, fr, cs, en])
        test (Just "it") Nothing           (it :| [en])
        test Nothing     (Just "it:cs")    (en :| [])
        test Nothing     Nothing           (en :| [])

    , testCase "Handling empty values" do
        test (Just "")   (Just "")         (en :| [])
        test (Just "")   (Just "it:cs:de") (en :| [])
        test (Just "cs") (Just "")         (cs :| [en])

    , testCase "Handling territories in locales" do
        test (Just "cs_CZ") (Just "it:cs:de")    (it :| [cs, de, cs_CZ, en])
        test (Just "en_GB") (Just "it:cs:de")    (it :| [cs, de, en_GB, en])
        test (Just "en_GB") Nothing              (en_GB :| [en])
        test (Just "en_GB") (Just "cs_CZ:en_GB") (cs_CZ :| [cs, en_GB, en])
        test (Just "en_GB") (Just "en_GB:en_CA") (en_GB :| [en_CA, en])
    ]
  where
    test
        :: HasCallStack
        => Maybe Text
        -> Maybe Text
        -> NonEmpty Locale
        -> IO ()
    test lang language expected = do
        let env :: HashMap Text Text
            env = HashMap.empty
                & maybe id (HashMap.insert "LANG")     lang
                . maybe id (HashMap.insert "LANGUAGE") language

        first show (parseEnv () env languagePreference) @?= Right expected

en, en_GB, en_CA, it, cs, cs_CZ, de, de_AT, fr :: Locale
en = defaultLocale
en_GB = Locale LanguageCode.EN (Just CountryCode.GB)
en_CA = Locale LanguageCode.EN (Just CountryCode.CA)
it = Locale LanguageCode.IT Nothing
cs = Locale LanguageCode.CS Nothing
cs_CZ = Locale LanguageCode.CS (Just CountryCode.CZ)
de = Locale LanguageCode.DE Nothing
de_AT = Locale LanguageCode.DE (Just CountryCode.AT)
fr = Locale LanguageCode.FR Nothing

expectParseFailure :: (HasCallStack, Show a) => Either e a -> IO ()
expectParseFailure = \case
    Left{} -> pure ()
    Right l -> assertFailure
        $ "Expected a parse failure, but got a success with: " <> show l
