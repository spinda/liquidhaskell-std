{-# LANGUAGE QuasiQuotes #-}

module LiquidHaskell.Qualifiers where

import LiquidHaskell

import Liquid.Prelude

[lq| qualif lq_qualif_bot_a    :: a    -> Bool |]
[lq| qualif lq_qualif_bot_bool :: Bool -> Bool |]
[lq| qualif lq_qualif_bot_int  :: Int  -> Bool |]

lq_qualif_bot_a    _ = 0 == 1
lq_qualif_bot_bool _ = 0 == 1
lq_qualif_bot_int  _ = 0 == 1

[lq| qualif lq_qualif_lt_z  |]
[lq| qualif lq_qualif_lte_z |]
[lq| qualif lq_qualif_gt_z  |]
[lq| qualif lq_qualif_gte_z |]
[lq| qualif lq_qualif_eq_z  |]
[lq| qualif lq_qualif_neq_z |]

lq_qualif_lt_z  v = v <  0
lq_qualif_lte_z v = v <= 0
lq_qualif_gt_z  v = v >  0
lq_qualif_gte_z v = v >= 0
lq_qualif_eq_z  v = v == 0
lq_qualif_neq_z v = v /= 0

[lq| qualif lq_qualif_lt  |]
[lq| qualif lq_qualif_lte |]
[lq| qualif lq_qualif_gt  |]
[lq| qualif lq_qualif_gte |]
[lq| qualif lq_qualif_eq  |]
[lq| qualif lq_qualif_neq |]

lq_qualif_lt  v x = v <  x
lq_qualif_lte v x = v <= x
lq_qualif_gt  v x = v >  x
lq_qualif_gte v x = v >= x
lq_qualif_eq  v x = v == x
lq_qualif_neq v x = v /= x

[lq| qualif lq_qualif_one   :: Int  -> Bool |]
[lq| qualif lq_qualif_true  :: Bool -> Bool |]
[lq| qualif lq_qualif_false :: Bool -> Bool |]

lq_qualif_one   v = v == 1
lq_qualif_true  v = v
lq_qualif_false v = not v

