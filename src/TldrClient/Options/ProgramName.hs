{-# LANGUAGE StrictData #-}
-- |
-- Module:      TldrClient.Options.ProgramName
-- Description: Data type representing program/command name of the tldr-pages
--              client
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type representing program\/command name of the tldr-pages client.
module TldrClient.Options.ProgramName
    ( ProgramName(..)
    , toString
    )
  where

import Data.String (String, )
import Data.Semigroup ((<>), )


-- | Program\/command name of the tldr-pages client.
data ProgramName
    = StandaloneApplication String
    -- ^ Executable \/ command name.
    | CommandWrapperSubcommand String String
    -- ^ Toolset and subcommand name.

toString :: ProgramName -> String
toString = \case
    StandaloneApplication command ->
        command
    CommandWrapperSubcommand toolset subcommand ->
        toolset <> " " <> subcommand
