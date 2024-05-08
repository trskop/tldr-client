-- |
-- Module:      TldrClient.Options.Shell
-- Description: Enum for supported shells
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Enum for supported shells along with utility functions.
module TldrClient.Options.Shell
    ( Shell(..)
    , values
    , parse
    )
  where

import Prelude (Bounded, Enum, maxBound, minBound, )

import Data.Either (Either(Left, Right), )
import Data.Eq (Eq, )
import Data.Functor ((<&>), )
import Data.List qualified as List (intercalate, lookup, )
import Data.Maybe (maybe, )
import Data.Semigroup ((<>), )
import Data.String (IsString, String, )
import Text.Show (Show, )

import Data.CaseInsensitive (CI, )
import Data.CaseInsensitive qualified as CI (FoldCase, mk, )

import Data.List.Compat (List, )


data Shell = Bash | Fish | Zsh
  deriving stock (Bounded, Enum, Eq, Show)

values :: List Shell
values = [minBound..maxBound]

toString :: IsString s => Shell -> s
toString = \case
    Bash -> "Bash"
    Fish -> "Fish"
    Zsh  -> "Zsh"

parse
    :: forall s
    .  (IsString s, Eq s, CI.FoldCase s)
    => s
    -> Either String Shell
parse s = maybe (Left unknownShell) Right (CI.mk s `List.lookup` values')
  where
    values' :: List (CI s, Shell)
    values' = values <&> \shell ->
        (toString shell, shell)

    unknownShell :: String
    unknownShell = "Unrecognised shell name, expected '"
        <> List.intercalate "', '" (values <&> toString) <> "'"
