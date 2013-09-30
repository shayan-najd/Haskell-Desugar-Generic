-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Desugaring
-- Copyright   :  (c) Shayan Najd
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Shayan Najd, shayan@chalmers.se
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Exts.Desugar.Generic 
       (module Language.Haskell.Exts.Unique
       ,module Language.Haskell.Exts.Desugar.Basic
       ,module Language.Haskell.Exts.Desugar.Generic.Type
       ,module Language.Haskell.Exts.Desugar.Generic.Pattern
       ,module Language.Haskell.Exts.Desugar.Generic.Case
       ,module Language.Haskell.Exts.Desugar.Conversion
       ,module Language.Haskell.Exts.Desugar.Generic.Expression
       ,module Language.Haskell.Exts.Desugar.Generic.Others
       ,module Language.Haskell.Exts.Desugar.Generic.Declaration
       ,module Language.Haskell.Exts.SimpleGenerics
       ) where


import Language.Haskell.Exts.Unique
import Language.Haskell.Exts.Desugar.Generic.Others
import Language.Haskell.Exts.Desugar.Basic
import Language.Haskell.Exts.Desugar.Generic.Type
import Language.Haskell.Exts.Desugar.Generic.Case
import Language.Haskell.Exts.Desugar.Generic.Expression
import Language.Haskell.Exts.Desugar.Generic.Pattern
import Language.Haskell.Exts.Desugar.Generic.Declaration
import Language.Haskell.Exts.Desugar.Conversion
import Language.Haskell.Exts.SimpleGenerics