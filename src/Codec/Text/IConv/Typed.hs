{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Codec.Text.IConv.Typed where

import qualified Codec.Text.IConv as I
import           Codec.Text.IConv.Typed.TH
import           Data.ByteString.Lazy
import GHC.TypeLits

$(generateEncodings)

convert :: ( KnownSymbol k1
            , KnownSymbol k2
            , ValidEncoding k1 ~ 'True
            , ValidEncoding k2 ~ 'True
            )
         => E k1
         -> E k2
         -> ByteString
         -> ByteString
convert e1 e2 input = I.convert (reifyEncoding e1) (reifyEncoding e2) input
