{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
module Codec.Text.IConv.Typed.TH where

#if defined darwin_HOST_OS
import Codec.Text.IConv.Typed.MacOSX
#else
import Codec.Text.IConv.Typed.Unix
#endif

import GHC.TypeLits
import Language.Haskell.TH

data E (k :: Symbol) = E

reifyEncoding :: KnownSymbol k => E k -> String
reifyEncoding = symbolVal

--------------------------------------------------------------------------------
generateEncodings :: Q [Dec]
generateEncodings = do
  let encs   = getAvailableEncodings
  let k      = mkName "k"
  let symbol = mkName "Symbol"
  let validEncoding = mkName "ValidEncoding"
  let bool = mkName "Bool"
#if __GLASGOW_HASKELL__ >= 800
  let instances = flip map encs $ \eName -> do
        return $ TySynEqn [LitT (StrTyLit eName)] (PromotedT (mkName "True"))
  (: []) <$> closedTypeFamilyD validEncoding [KindedTV k (ConT symbol)] (KindSig (ConT bool)) Nothing instances
#else
  let instances = flip map encs $ \eName -> TySynEqn [LitT (StrTyLit eName)] (PromotedT (mkName "True"))
  return [ClosedTypeFamilyD validEncoding [KindedTV k (ConT symbol)] (Just $ (ConT bool)) instances]
#endif
