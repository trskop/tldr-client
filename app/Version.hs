module Version
    ( VersionInfo(..)
    , prettyVersionInfo
    )
  where

import Data.Eq ((==))
import Data.Version (Version, makeVersion, showVersion)

import Prettyprinter (Doc, (<+>), pretty, vsep)


data VersionInfo = VersionInfo
    { clientVersion :: Version
    , tldrClientSpecificationVersion :: Version
    }

prettyVersion :: Version -> Doc ann
prettyVersion v =
    if v == makeVersion []
        then "N/A"
        else pretty (showVersion v)

prettyVersionInfo :: VersionInfo -> Doc ann
prettyVersionInfo VersionInfo{..} = vsep
    [ "Client version:                    "
        <+> prettyVersion clientVersion
    , "Tldr Client Specification Version: "
        <+> prettyVersion tldrClientSpecificationVersion
    , ""
    ]
