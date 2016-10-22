{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Codec.Text.IConv.Typed where

import qualified Codec.Text.IConv as I
import           Codec.Text.IConv.Typed.TH
import           Data.ByteString.Lazy

$(generateEncodings)

type family AvailableEncoding (k :: *) :: Bool where
  AvailableEncoding UTF8 = 'True


{-
convert "UTF-8" "LATIN1"
-}

convert :: ( AvailableEncoding e1 ~ 'True
           , AvailableEncoding e2 ~ 'True
           , Show e1
           , Show e2
           )
        => e1
        -> e2
        -> ByteString
        -> ByteString
convert e1 e2 input = I.convert (show e1) (show e2) input
