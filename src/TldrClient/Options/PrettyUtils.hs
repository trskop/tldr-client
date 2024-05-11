{-# LANGUAGE StrictData #-}
-- |
-- Module:      TldrClient.Options.PrettyUtils
-- Description: Utilities for constructing help messages
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities for constructing help messages.
module TldrClient.Options.PrettyUtils
    ( PrettyUtils(..)
    , mkPrettyUtils
    )
  where

import Data.Char (Char, )
import Data.Foldable (foldMap, )
import Data.Function ((.), )
import Data.Functor (fmap, )
import Data.Semigroup ((<>), )
import Data.String (String, )
import Data.String qualified as String (words, )

import Options.Applicative.Help qualified as Options
    ( Doc
    , bold
    , brackets
    , dullgreen
    , encloseSep
    , fillSep
    , magenta
    , string
    , underline
    , )

import Data.List.Compat (List, )


data PrettyUtils = PrettyUtils
    { list :: List Options.Doc -> Options.Doc
    , flagDoc :: Options.Doc -> Options.Doc
    , flag :: String -> Options.Doc
    , shortFlag :: Char -> Options.Doc
    , opt :: String -> String -> Options.Doc
    , shortOpt :: Char -> String -> Options.Doc
    , paragraph :: String -> Options.Doc
    , metavar :: Options.Doc -> Options.Doc
    , value :: Options.Doc -> Options.Doc
    , alt :: List Options.Doc -> Options.Doc
    , ellipsis :: Options.Doc
    }

-- | Construct pretty printing utilities based on configuration.
mkPrettyUtils
    ::  ( (Options.Doc -> Options.Doc)
        -> (Options.Doc -> Options.Doc)
        -> Options.Doc -> Options.Doc
        )
    -- ^ Apply colour and\/or decoration.
    -> (Options.Doc -> Options.Doc -> Options.Doc)
    -- ^ Use either unicode or ascii.
    -> PrettyUtils
mkPrettyUtils applyColourAndDecoration unicode = PrettyUtils{..}
  where
    list :: List Options.Doc -> Options.Doc
    list = Options.encloseSep "" "" ", "

    flagDoc :: Options.Doc -> Options.Doc
    flagDoc = applyColourAndDecoration Options.dullgreen Options.bold

    flag :: String -> Options.Doc
    flag s = flagDoc (Options.string ("--" <> s))

    shortFlag :: Char -> Options.Doc
    shortFlag c = flagDoc (Options.string ['-', c])

    opt :: String -> String -> Options.Doc
    opt o v = flag o <> "=" <> metavar (Options.string v)

    shortOpt :: Char -> String -> Options.Doc
    shortOpt o v = shortFlag o <> " " <> metavar (Options.string v)

    paragraph :: String -> Options.Doc
    paragraph = Options.fillSep . fmap Options.string . String.words

    metavar :: Options.Doc -> Options.Doc
    metavar = applyColourAndDecoration Options.dullgreen Options.underline

    value :: Options.Doc -> Options.Doc
    value = applyColourAndDecoration Options.magenta Options.underline

    alt :: List Options.Doc -> Options.Doc
    alt = \case
        [] -> ""
        d : ds -> d <> foldMap ("|" <>) ds

    ellipsis :: Options.Doc
    ellipsis = Options.brackets (unicode "…" "...")
