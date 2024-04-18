-- |
-- Module:      Dhall.Marshal.Decode.Extended
-- Description: Dhall decoding utilities
-- Copyright:   (c) 2021-2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Dhall decoding utilities.
module Dhall.Marshal.Decode.Extended
    ( module Dhall.Marshal.Decode
    , enum
    )
  where

import Control.Applicative (pure, )
import Data.Functor ((<$), )
import Data.Maybe (Maybe(Nothing), )
import Data.Maybe qualified (maybe, )

--import Data.Text (Text, )
import Dhall.Core qualified as Dhall (Expr(Field, Union), fieldSelectionLabel, )
import Dhall.Map qualified (lookup, )
import Dhall.Map qualified as Dhall (Map)
import Dhall.Marshal.Decode


enum :: Dhall.Map Text a -> Decoder a
enum values = Decoder
    { extract = \expr -> case expr of
        Dhall.Field (Dhall.Union _) (Dhall.fieldSelectionLabel -> fld) ->
            Data.Maybe.maybe (typeError (pure unionType) expr) pure
                (Dhall.Map.lookup fld values)
        _ ->
            typeError (pure unionType) expr

    , expected = pure unionType
    }
  where
    unionType :: forall s a. Dhall.Expr s a
    unionType = Dhall.Union (Nothing <$ values)
{-# INLINEABLE enum #-}
