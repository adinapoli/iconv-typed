{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = print $ convert (E :: E "UTF-8") (E :: E "LATIN1") "hello"
