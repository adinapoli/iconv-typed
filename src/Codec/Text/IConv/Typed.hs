{-# LANGUAGE TemplateHaskell #-}
module Codec.Text.IConv.Typed where

import Codec.Text.IConv.Typed.TH

$(generateEncodings)
