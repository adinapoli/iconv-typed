{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Codec.Text.IConv.Typed.TH where

#if defined darwin_HOST_OS
import           Codec.Text.IConv.Typed.MacOSX
#else
import           Codec.Text.IConv.Typed.Unix
#endif

import           Codec.Text.IConv
import           Control.Monad
import           Data.List (foldl')
import           Data.Maybe
import qualified Data.Text as T
import           Language.Haskell.TH
import           Text.Read


filterNumericEncoding :: [EncodingName] -> [EncodingName]
filterNumericEncoding = filter (not . numerical)
  where
    numerical x = isJust (readMaybe x :: Maybe Int)

--------------------------------------------------------------------------------
replacementRules :: [(T.Text, T.Text)]
replacementRules = [ ("-", "__")
                   , (".", "____")
                   , (":", "___")
                   ]

--------------------------------------------------------------------------------
applyRule :: T.Text -> (T.Text, T.Text) -> T.Text
applyRule t (input, output) = T.replace input output t

--------------------------------------------------------------------------------
convertName :: EncodingName -> EncodingName
convertName = T.unpack . (\t -> foldl' applyRule t replacementRules) . T.pack

--------------------------------------------------------------------------------
generateEncodings :: Q [Dec]
generateEncodings = do
  let encs = filterNumericEncoding getAvailableEncodings
  mconcat <$> (forM encs $ \e -> do
    let eName = mkName (convertName e)
    let showI = mkName "Show"
    return [DataD mempty eName mempty Nothing [NormalC eName mempty] [ConT showI]])
