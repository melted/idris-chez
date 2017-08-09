{-# LANGUAGE OverloadedStrings #-}

module Chez.Codegen(codegenChez) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT as TT
import Idris.Core.CaseTree

import Data.Bits
import Data.List
import Data.Maybe
import Data.Char
import Data.String(IsString, fromString)

import qualified Data.Text as T

import Chez.Compatibility (fixup, intercept)
import Chez.Operators
import Chez.Util

import Paths_idris_chez


codegenChez :: CodeGenerator
codegenChez ci = do let decls = fixup (simpleDecls ci)
                    let out = map doCodegen decls ++ [start]++["(exit 0)\n"]
                    let code = concat out
                    dir <- getDataDir
                    let top = "#!/usr/bin/env scheme-script\n" ++
                              "#!chezscheme\n" ++
                              "(import (chezscheme))\n"
                    rtslib <- readFile $ dir ++ "/rts/rts.ss"
                    writeFile (outputFile ci) (top ++ rtslib ++ code)

start :: String
start = "(" ++ sname (MN 0 "runMain") ++ ")"


doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun n' as locs exp) = 
    defineFun (sname n) (map (loc . fst) (zip [0..] as)) (compileExpr exp)

compileExpr :: SExp -> String
compileExpr (SV v) = compileVar v
compileExpr (SApp _ n args) = call (sname n) (compileVars args)
compileExpr (SLet var exp body) = slet (compileVar var) (compileExpr exp) (compileExpr body)
compileExpr (SUpdate var exp) = compileExpr exp
-- TODO: SCon check for scheme primitive types and use them instead
compileExpr (SCon _ t n xs) = sexp ("list":show t:compileVars xs)
compileExpr (SCase ctype var alts) = compileCase var alts
compileExpr (SChkCase var alts) = compileCase var alts
compileExpr (SProj var i) = sexp ["list-ref", compileVar var, show i]
compileExpr (SConst c) = compileConst c
compileExpr (SForeign name ret args) = compileForeign name ret args `fromMaybe`
                                            intercept name ret args
compileExpr (SOp prim args) = compileOp prim args
compileExpr SNothing = "'()"
compileExpr (SError what) = sexp ["error", sstr "idris", sstr what]



-- TODO: Add case where all alts are const
-- TODO: if-stat for 1 alt plus default?
compileCase :: LVar -> [SAlt] -> String
compileCase var alts = cond $ map (compileAlt var) (salts alts)
    where
        salts [] = []
        salts (sd@(SDefaultCase _):_) = [sd]
        salts (x:xs) = x:(salts xs)


-- TODO: Special case scheme primitive types
compileAlt :: LVar -> SAlt -> String
compileAlt var (SConCase lv t n args body) = sexp [call "=" [car $ compileVar var,show t], project 1 lv args body]
    where
        project i v ns body = apply (lambda (map (loc . fst) (zip [v..] ns)) (compileExpr body)) (cdr $ compileVar var) 
compileAlt var (SConstCase c body) = sexp [compileCompare var c, compileExpr body]
compileAlt _ (SDefaultCase body) = sexp ["else", compileExpr body]

compileCompare :: LVar -> Const -> String
compileCompare var c@(Ch _) = call "char=?" [compileVar var, compileConst c]
compileCompare var c@(Str _) = call "string=?" [compileVar var, compileConst c]
compileCompare var c = call "=" [compileVar var, compileConst c] 

compileConst :: Const -> String
compileConst (I i) = show i
compileConst (BI bi) = show bi
compileConst (Fl d) = show d
compileConst (Ch c) = schemeChar c
compileConst (Str s) = schemeString s
compileConst (B8 w) = show w
compileConst (B16 w) = show w
compileConst (B32 w) = show w
compileConst (B64 w) = show w
-- type const, won't be used, hopefully
compileConst t | isTypeConst t = "#f"
compileConst x = error $ "Unimplemented const " ++ show x



compileForeign :: FDesc -> FDesc -> [(FDesc, LVar)] -> String
compileForeign rty (FStr name) args = sexp $ [call "foreign-procedure"
                                         [name, sexp (map (ffiType . fst) args), ffiType rty]] ++
                                          map (compileVar . snd) args
compileForeign _ _ _ = error "Illegal ffi call"
