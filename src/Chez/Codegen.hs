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
                    let init = initCall (compileLibs ci)
                    let include = intercalate "\n" (includes ci)
                    let code = concat out
                    dir <- getDataDir
                    let top = "#!/usr/bin/env scheme-script\n" ++
                              "#!chezscheme\n" ++
                              "(import (chezscheme))\n"
                    rtslib <- readFile $ dir ++ "/rts/rts.ss"
                    writeFile (outputFile ci) (top ++ rtslib ++ init ++ include ++ code)

initCall :: [String] -> String
initCall libs = call "idris-chez-init" [call "list" (map sstr libs)] ++ "\n"

start :: String
start = "(" ++ sname (MN 0 "runMain") ++ ")\n"


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
compileExpr (SForeign ret name args) = handleForeign ret name args
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


handleForeign ret name args = if isCType ret
                                 then compileForeign ret name args `fromMaybe` intercept ret name args
                                 else compileSchemeForeign ret name args 

compileForeign :: FDesc -> FDesc -> [(FDesc, LVar)] -> String
compileForeign rty (FStr ('&':name)) [] = call "foreign-entry" [sstr name]
compileForeign rty (FStr "%dynamic") (f:args) = foreignProcedure (compileVar $ snd f) rty args
compileForeign rty (FStr "%wrapper") (f:args) = makeWrapper (fst f) (compileVar (snd f))
compileForeign rty (FStr name) args = foreignProcedure (sstr name) rty args

compileForeign _ _ _ = error "Illegal ffi call"

foreignProcedure proc rty args = handleFFIReturn (toFType rty) $
    sexp $ [call "foreign-procedure" [proc, sexp (map (ffiType . fst) args), ffiType rty]]
        ++ map compileFFIVar args

compileFFIVar (fd, x) | isFunction fd = makeWrapper fd (compileVar x)
compileFFIVar (_,x) = compileVar x

handleFFIReturn :: FType -> String -> String
handleFFIReturn FUnit s = call "begin" [s, "'()"]
handleFFIReturn _ s = s

makeWrapper fd s = slet "ff" callable body  
  where
    (rty, args) = getSignature fd
    callable = call "foreign-callable" [wrapper, sexp args, rty]
    wrapper = call "idris-chez-make-wrapper" [s] 
    body = call "lock-object" ["ff"] ++ call "foreign-callable-entry-point" ["ff"]

-- Scheme FFI

compileSchemeForeign ret (FStr name) args = handleSchemeReturn ret (call name (map compileSchemeVar args))

-- TODO: If there are datatypes that need to translated when passing to Scheme, do it here
compileSchemeVar :: (FDesc, LVar) -> String
compileSchemeVar (FCon c, x) | c == sUN "S_Bool" = call "=" ["1",call "car" [compileVar x]] 
compileSchemeVar (_, x) = compileVar x

-- TODO: If any return types needs marshalling, do it here
handleSchemeReturn :: FDesc -> String -> String
handleSchemeReturn (FCon c) s | c == sUN "S_Bool" = call "list" [call "if" [s, "1", "0"]]
handleSchemeReturn ret s = s