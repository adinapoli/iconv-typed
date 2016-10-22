{-# LANGUAGE OverloadedStrings #-}
module Codec.Text.IConv.Typed.Unix
  ( getAvailableEncodings
  ) where

import Codec.Text.IConv (EncodingName)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Shelly

getAvailableEncodings :: [EncodingName]
getAvailableEncodings = unsafePerformIO $ shelly $ silently $ escaping False $ do
  map T.unpack . T.splitOn "," . T.strip <$> run "iconv" ["-l"]
{-# NOINLINE getAvailableEncodings #-}
