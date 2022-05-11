{-# LANGUAGE Trustworthy #-}
{-|
An auxiliary module that exports the 'IsList' class from "GHC.Exts". We use
this intermediate module to isolate a safe feature from an otherwise non-safe
module, and prevent all of "Data.Bimap" from being marked as not safe just
because we are importing "GHC.Exts".

The module only exports a class, and the class does not define any methods in
an unsafe way. We therefore consider it safe and mark this module as
trustworthy.
-}
module Data.BimapExt (
    IsList(..)
) where

import GHC.Exts (IsList(..))
