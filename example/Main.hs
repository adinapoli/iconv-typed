{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = do
  let res = convert (E :: E "UTF-8") (E :: E "LATIN1") "hello"
  print res
