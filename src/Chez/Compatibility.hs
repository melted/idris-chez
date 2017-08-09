

module Chez.Compatibility (fixup, intercept) where

import Idris.Core.TT

import IRTS.Lang
import IRTS.Simplified

import Chez.Util

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

-- Change the sdecls, this is used when we need
-- to be aware of the context.
fixup :: [(Name, SDecl)] -> [(Name, SDecl)]
fixup decls = map rewriteDecl decls

-- Check a foreign call, and return substitute code if a match
intercept :: FDesc -> FDesc -> [(FDesc, LVar)] -> Maybe String
-- isNull used from Prelude.Strings
intercept _ (FStr "isNull") [(_, p)] = Just $ predicate (call "=" ["0", compileVar p])
-- Prelude.File
intercept _ (FStr "fileOpen") [(_, _), (_, file), (_, mode)] = 
                            Just $ call "idris-chez-fileopen" [compileVar file, compileVar mode]
intercept _ (FStr "fileSize") [(_, _), (_, p)] = Just $ call "file-length" [compileVar p]
-- TODO: showerror and mkFileError should be better
intercept _ (FStr "idris_showerror") [(_, _), (_, i)] = Just $ sstr "Unhelpful error"
intercept _ (FStr "idris_mkFileError") [(_, _), (_, p)] = Just $ sexp ["0", "0"]

intercept _ _ _ = Nothing

rewriteDecl :: (Name, SDecl) -> (Name, SDecl)
rewriteDecl (n, sd) | n == sUN "Prelude.File.fgetc" = (n, rewriteFgetc sd)
rewriteDecl d = d

-- Yes, this machinery basically exists so we can rewrite the fgetc FFI
-- descriptor to contain another, less common, name.
-- Oh, how I wish SExpr had derived Generic.
rewriteFgetc sd = rewriteFFI "fgetc" "idris_fgetc" sd

rewriteFFI :: String -> String -> SDecl -> SDecl
rewriteFFI from to (SFun a b c exp) = SFun a b c (rewriteExp exp)
    where
        rewriteExp (SForeign r (FStr from) a) = SForeign r (FStr to) a
        rewriteExp (SLet v exp body) = SLet v (rewriteExp exp) (rewriteExp body)
        rewriteExp (SCase a b alts) = SCase a b (map rewriteAlt alts)
        rewriteExp (SChkCase a alts) = SChkCase a (map rewriteAlt alts)
        rewriteExp e = e

        rewriteAlt (SConCase a b c d exp) = SConCase a b c d (rewriteExp exp)
        rewriteAlt (SConstCase a exp) = SConstCase a (rewriteExp exp)
        rewriteAlt (SDefaultCase exp) = SDefaultCase (rewriteExp exp)