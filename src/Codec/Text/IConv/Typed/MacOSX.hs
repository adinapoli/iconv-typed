{-# LANGUAGE OverloadedStrings #-}
module Codec.Text.IConv.Typed.MacOSX
  ( getAvailableEncodings
  ) where

import Codec.Text.IConv (EncodingName)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Shelly

getAvailableEncodings :: [EncodingName]
getAvailableEncodings = unsafePerformIO $ shelly $ silently $ escaping False $ do
  map T.unpack . mconcat . map T.words . T.lines . T.strip <$> run "iconv" ["-l"]
{-# NOINLINE getAvailableEncodings #-}
