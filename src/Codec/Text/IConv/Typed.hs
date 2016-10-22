{-# LANGUAGE CPP #-}
module Codec.Text.IConv.Typed (
  getAvailableEncodings
  ) where

#if defined darwin_HOST_OS
import Codec.Text.IConv.Typed.MacOSX
#else
import Codec.Text.IConv.Typed.Unix
#endif
