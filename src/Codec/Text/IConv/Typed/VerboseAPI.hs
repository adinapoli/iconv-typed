{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Codec.Text.IConv.Typed.VerboseAPI
  ( E(..)
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
import           Data.ByteString.Lazy
import           GHC.TypeLits

$(generateEncodings)

--------------------------------------------------------------------------------
convert :: (  KnownSymbol k1
           , KnownSymbol k2
           , ValidEncoding k1 ~ 'True
           , ValidEncoding k2 ~ 'True
           )
         => E k1       -- ^ Name of input string encoding
         -> E k2       -- ^ Name of output string encoding
         -> ByteString -- ^ Input text
         -> ByteString -- ^ Output text
convert e1 e2 input = I.convert (reifyEncoding e1) (reifyEncoding e2) input

--------------------------------------------------------------------------------
convertFuzzy :: ( KnownSymbol k1
                , KnownSymbol k2
                , ValidEncoding k1 ~ 'True
                , ValidEncoding k2 ~ 'True
                )
             => I.Fuzzy -- ^ Whether to try and transliterate or
                        -- discard characters with no direct conversion
             -> E k1    -- ^ Name of input string encoding
             -> E k2    -- ^ Name of output string encoding
             -> ByteString    -- ^ Input text
             -> ByteString    -- ^ Output text
convertFuzzy fuzzy fromEncoding toEncoding input =
  I.convertFuzzy fuzzy (reifyEncoding fromEncoding) (reifyEncoding toEncoding) input

--------------------------------------------------------------------------------
convertStrictly :: ( KnownSymbol k1
                   , KnownSymbol k2
                   , ValidEncoding k1 ~ 'True
                   , ValidEncoding k2 ~ 'True
                   )
                => E k1                                -- ^ Name of input string encoding
                -> E k2                                -- ^ Name of output string encoding
                -> ByteString                          -- ^ Input text
                -> Either ByteString I.ConversionError -- ^ Output text or conversion error
convertStrictly fromEncoding toEncoding input =
  I.convertStrictly (reifyEncoding fromEncoding) (reifyEncoding toEncoding) input

--------------------------------------------------------------------------------
convertLazily :: ( KnownSymbol k1
                 , KnownSymbol k2
                 , ValidEncoding k1 ~ 'True
                 , ValidEncoding k2 ~ 'True
                 )
              => E k1  -- ^ Name of input string encoding
              -> E k2  -- ^ Name of output string encoding
              -> ByteString  -- ^ Input text
              -> [I.Span]        -- ^ Output text spans
convertLazily fromEncoding toEncoding input = I.convertLazily (reifyEncoding fromEncoding) (reifyEncoding toEncoding) input
