{-# LANGUAGE QuasiQuotes #-}

module LiquidHaskell.Prelude (
    Nat
  , len
  , liquidAssert
  , liquidAssertB
  ) where

import LiquidHaskell

import Liquid.GHC.Base
import Liquid.GHC.Num

[lq| type Nat = {v:Int | 0 <= v} |]

[lq| measure len :: [a] -> Int |]
len []     = 0
len (_:xs) = 1 + len xs

[lq| assume liquidAssert :: {v:Bool | v} -> a -> a |]
liquidAssert _ x = x
{-# NOINLINE liquidAssert #-}

[lq| assume liquidAssertB :: x:{v:Bool | v} -> {v: Bool | x} |]
liquidAssertB b = b
{-# NOINLINE liquidAssertB #-}

