{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = print $ convert @"UTF-8" @"LATIN1" "hello"
