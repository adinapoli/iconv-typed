{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = do
  let res1 = convert (E @"UTF-8") (E @"LATIN1") "hello"
  let res2 = convert' @"UTF-8" @"LATIN1" "hello"
  print res1
  print res2
