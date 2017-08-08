

module Chez.Compatibility (fixup) where

import Idris.Core.TT

import IRTS.Simplified

-- This module is where we sweep all the ugly things under the rug.
-- The aim of idris-chez is to be fully compatible with the C backend,
-- but as some functionality in the libraries depend on the C rts or
-- C standard lib functions we have a choice to make: implement those in
-- C or make them in Scheme and patch the call points to point right
-- at compilation time.
-- We will use both approaches. The Prelude.File library should use
-- the Scheme functions for file handling. The CFFI usage of "malloc"
-- and friends should as clearly be left as is.

-- TODO: implement it
fixup :: [(Name, SDecl)] -> [(Name, SDecl)]
fixup decls = decls

