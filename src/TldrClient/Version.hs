-- |
-- Module:      TldrClient.Version
-- Description: Version information printed by `--version` flag
-- Copyright:   (c) 2021, 2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Version information printed by @--version@ flag.
module TldrClient.Version
    ( VersionInfo(..)
    , prettyVersionInfo
    )
  where

import Data.Eq ((==), )
import Data.Function (($), )
import Data.Functor ((<&>), )
import Data.Maybe (Maybe(Just), catMaybes, )
import Data.Version (Version, makeVersion, showVersion, )

import Prettyprinter (Doc, (<+>), pretty, vsep, )


data VersionInfo = VersionInfo
    { clientVersion :: Version
    , subcommandProtocolVersion :: Maybe Version
    , tldrClientSpecificationVersion :: Version
    , dhallLibraryVersion :: Version
    }

prettyVersion :: Version -> Doc ann
prettyVersion v =
    if v == makeVersion []
        then "N/A"
        else pretty (showVersion v)

prettyVersionInfo :: VersionInfo -> Doc ann
prettyVersionInfo VersionInfo{..} = vsep $ catMaybes
    [ Just $ clientVersionHeader <+> prettyVersion clientVersion
    , subcommandProtocolVersion <&> \v ->
        subcommandProtocolVersionHeader <+> prettyVersion v
    , Just $ tldrClientSpecificationVersionHeader
        <+> prettyVersion tldrClientSpecificationVersion
    , Just $ dhallLibraryVersionHeader <+> prettyVersion dhallLibraryVersion
    , Just ""
    ]
  where
    clientVersionHeader =
        "Client version:                    "
    subcommandProtocolVersionHeader =
        "Subcommand Protocol Version:       "
    tldrClientSpecificationVersionHeader =
        "Tldr Client Specification Version: "
    dhallLibraryVersionHeader =
        "Dhall Library Version:             "
