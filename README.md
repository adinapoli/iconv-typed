

## iconv-typed

An experiment in bringing type safety to the [iconv](http://hackage.haskell.org/package/iconv) package.

This is _almost_ a drop-in replacement. Compare the original code from `iconv`:

``` haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv

main :: IO ()
main = print $ convert "UTF-8" "LATIN1" "hello"
```

With the equivalent in `iconv-typed`:

``` haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = print $ convert @"UTF-8" @"LATIN1" "hello"
```

As a result, this code will compile and run only if the passed encoding resolves to a supported
encoding (as retrieved at compile time by calling `iconv -l`). For example, the following won't compile:

``` haskell
main = print $ convert @"UFT-8" @"LATIN1" "hello"
```

As `UFT` is mispelled.

Using GHC < 8.0 that doesn't supports `TypeInType`? No problem, we've got you covered!

``` haskell
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = print $ convert (E :: E "UTF-8") (E :: E "LATIN1") "hello"
``` haskell
