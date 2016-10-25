{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
module Codec.Text.IConv.Typed
  ( E(..)
#if __GLASGOW_HASKELL__ > 800
  , Enc(..)
#endif
  , ValidEncoding
  , convert
  , convertFuzzy
  , convertStrictly
  , convertLazily
  -- * Handy re-exports
  , I.reportConversionError
  , I.Fuzzy(..)
  , I.ConversionError(..)
  , I.Span(..)
  ) where

import qualified Codec.Text.IConv as I
import           Codec.Text.IConv.Typed.TH

#if __GLASGOW_HASKELL__ > 800
import           Codec.Text.IConv.Typed.TypeInTypeAPI
#else
import           Codec.Text.IConv.Typed.VerboseAPI
#endif
