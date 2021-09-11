module Data.Nat
  ( -- * Base types
    Internal.Nat8,
    Internal.Nat16,
    Internal.Nat32,
    Internal.Nat64,
    Internal.Nat,

    -- * Helper types
    Internal.NatSum (..),
    Internal.NatProduct (..),
  )
where

import qualified Data.Nat.Internal as Internal