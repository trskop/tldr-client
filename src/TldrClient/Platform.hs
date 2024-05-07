-- |
-- Module:      TldrClient.Platform
-- Description: Data types that represent a system platform
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Tldr pages client logic.
module TldrClient.Platform
    ( SomePlatform(..)
    , Platform(..)
    , parse
    , current
    )
  where

import Data.Eq (Eq, )
import Data.Function (($), )
import Data.Maybe (fromMaybe, )
import Data.String (IsString, fromString, )
import Text.Show (Show, )
import Data.List qualified as List (lookup, )
import System.Info qualified (os, )

import Data.CaseInsensitive (CI, )
import Data.CaseInsensitive qualified as CI (FoldCase, foldCase, foldedCase, )


data SomePlatform s
    = AllPlatforms
    | KnownPlatform Platform
    | OtherPlatform s
  deriving stock (Eq, Show)

-- | List of platforms as they are currently supported by tldr-pages, see
-- [github.com/tldr-pages/tldr](https://github.com/tldr-pages/tldr).
data Platform
    = Android
    | Freebsd
    | Linux
    -- ^ Any Linux distro
    | Netbsd
    | Openbsd
    | Macos
    -- ^ OSX\/Mac OS\/macOS
    --
    -- Currently the value @osx@ is used by the \"tldr pages\" project, but
    -- they want to move to @macos@ completely some time in the future.
    | Sunos
    | Windows
  deriving stock (Eq, Show)

parse :: forall s. (IsString s, Eq s, CI.FoldCase s) => s -> SomePlatform s
parse s = fromMaybe (OtherPlatform (CI.foldCase s)) $ s `List.lookup`
    [ ( "all",     AllPlatforms)
    , ( "android", KnownPlatform Android)
    , ( "freebsd", KnownPlatform Freebsd)
    , ( "linux",   KnownPlatform Linux)
    , ( "macos",   KnownPlatform Macos)
    , ( "netbsd",  KnownPlatform Netbsd)
    , ( "openbsd", KnownPlatform Openbsd)
    , ( "osx",     KnownPlatform Macos)
    , ( "sunos",   KnownPlatform Sunos)
    , ( "windows", KnownPlatform Windows)
    ]

current :: forall s. (IsString s, Eq s, CI.FoldCase s) => SomePlatform s
current = fromMaybe (OtherPlatform (CI.foldedCase os)) $ os `List.lookup`
    [ ( "darwin",        KnownPlatform Macos)
    , ( "freebsd",       KnownPlatform Freebsd)
    , ( "linux",         KnownPlatform Linux)
    , ( "linux-android", KnownPlatform Android)
    , ( "mingw32",       KnownPlatform Windows)
    , ( "netbsd",        KnownPlatform Netbsd)
    , ( "openbsd",       KnownPlatform Openbsd)
    ]
  where
    os :: CI s
    os = fromString System.Info.os
