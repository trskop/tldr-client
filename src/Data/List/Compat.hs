{-# LANGUAGE CPP #-}
-- |
-- Module:      Data.List.Compat
-- Description: List compatibility
-- Copyright:   (c) 2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- List compatibility layer.
module Data.List.Compat
    ( List
    )
  where

#if MIN_VERSION_base(4, 18, 0)
import GHC.List (List, )
#endif


#if !MIN_VERSION_base(4, 18, 0)
type List = []
#endif
