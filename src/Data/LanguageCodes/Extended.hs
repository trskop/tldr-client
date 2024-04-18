-- |
-- Module:      Data.LanguageCodes.Extended
-- Description: Adaptation module for "iso639" package
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Adaptation module for `iso639` package.
module Data.LanguageCodes.Extended
    ( LanguageCode
    , LanguageCode.ISO639_1(..)
    , fromText
    , toString
    , values

    -- * Dhall Encoding
    , decode
    , )
  where

import Data.Bifunctor (first, )
import Data.Bool (otherwise, )
import Data.Functor ((<$>), )
import Data.Maybe (Maybe(Just, Nothing), )
import Data.String (IsString, fromString, )

import Data.LanguageCodes (ISO639_1, )
import Data.LanguageCodes qualified as LanguageCode
import Data.Text (Text, )
import Data.Text qualified as Text (null, toUpper, uncons, )
import Dhall.Map qualified (fromList, )
import Dhall.Marshal.Decode.Extended qualified as Dhall (Decoder, enum, )


type LanguageCode = ISO639_1


toString :: IsString s => LanguageCode -> s
toString l =
    let (l1, l2) = LanguageCode.toChars l
    in  fromString [l1, l2]
{-# SPECIALIZE INLINE toString :: LanguageCode -> Text #-}

values :: IsString s => [(s, LanguageCode)]
values =
    -- ISO639_1 doesn't have `Bounded` instance.
    [(toString l, l) | l <- [LanguageCode.AA .. LanguageCode.ZU]]
{-# SPECIALIZE INLINE values :: [(Text, LanguageCode)] #-}

fromText :: Text -> Maybe LanguageCode
fromText t
  | Just (c₀, t₀) <- Text.uncons t
  , Just (c₁, t₁) <- Text.uncons t₀
  , Text.null t₁ =
        LanguageCode.fromChars c₀ c₁
  | otherwise =
        Nothing
{-# INLINEABLE fromText #-}

decode :: Dhall.Decoder LanguageCode
decode = Dhall.enum (Dhall.Map.fromList values')
  where
    values' = first Text.toUpper <$> values
{-# INLINEABLE decode #-}
