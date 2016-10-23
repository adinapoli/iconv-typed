{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
module Codec.Text.IConv.Typed.TH where

#if defined darwin_HOST_OS
import           Codec.Text.IConv.Typed.MacOSX
#else
import           Codec.Text.IConv.Typed.Unix
#endif

import           Codec.Text.IConv
import           Data.List (foldl')
import           Data.Maybe
import qualified Data.Text as T
import           GHC.TypeLits
import           Language.Haskell.TH
import           Text.Read

data E (k :: Symbol) = E

reifyEncoding :: KnownSymbol k => E k -> String
reifyEncoding = symbolVal

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

-- closedTypeFamilyD :: Name -> [TyVarBndr] -> FamilyResultSig -> Maybe InjectivityAnn -> [TySynEqnQ] -> DecQ


--------------------------------------------------------------------------------
generateEncodings :: Q [Dec]
generateEncodings = do
  let encs   = getAvailableEncodings
  let k      = mkName "k"
  let symbol = mkName "Symbol"
  let validEncoding = mkName "ValidEncoding"
  let bool = mkName "Bool"
  let instances = flip map encs $ \eName -> do
        return $ TySynEqn [LitT (StrTyLit eName)] (PromotedT (mkName "True"))
  (: []) <$> closedTypeFamilyD validEncoding [KindedTV k (ConT symbol)] (KindSig (ConT bool)) Nothing instances
