{-# LANGUAGE OverloadedStrings #-}

module IRTS.CodegenChez(codegenChez) where

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

import Paths_idris_chez


codegenChez :: CodeGenerator
codegenChez ci = do let out = map doCodegen (simpleDecls ci) ++ [start]
                    let code = concat out
                    dir <- getDataDir
                    let shebang = "#!/usr/bin/env scheme\n"
                    rtslib <- readFile $ dir ++ "/rts/rts.ss"
                    writeFile (outputFile ci) (shebang ++ rtslib ++ code)

start :: String
start = "(" ++ sname (MN 0 "runMain") ++ ")"


doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun n' as locs exp) = 
    defineFun (sname n) (map (loc . fst) (zip [0..] as)) (compileExpr exp)

compileExpr :: SExp -> String
compileExpr (SV v) = compileVar v
compileExpr (SApp _ n args) = call (sname n) (compileVars args)
compileExpr (SLet var exp body) = slet (compileVar var) (compileExpr exp) (compileExpr body)
compileExpr (SUpdate var exp) = sset (compileVar var) (compileExpr exp)
-- TODO: SCon check for scheme primitive types and use them instead
compileExpr (SCon _ t n xs) = sexp ("list":show t:compileVars xs)
compileExpr (SCase ctype var alts) = compileCase var alts
compileExpr (SChkCase var alts) = compileCase var alts
compileExpr (SProj var i) = sexp ["list-ref", compileVar var, show i]
compileExpr (SConst c) = compileConst c
compileExpr (SForeign name ret args) = compileForeign name ret args 
compileExpr (SOp prim args) = compileOp prim args
compileExpr SNothing = "'()"
compileExpr (SError what) = sexp ["error", "idris", what]

compileVar :: LVar -> String
compileVar (Loc i) = loc i
compileVar (Glob n) = sname n

compileVars = map compileVar

-- TODO: Add case where all alts are const
compileCase :: LVar -> [SAlt] -> String
compileCase var alts = cond $ map (compileAlt var) salts
    where
        salts = sortBy caseOrder alts
        caseOrder (SDefaultCase _) _ = GT
        caseOrder _ (SDefaultCase _) = LT
        caseOrder _ _ = EQ

-- TODO: Special case scheme primitive types
compileAlt :: LVar -> SAlt -> String
compileAlt var (SConCase lv t n args body) = sexp [call "=" [car $ compileVar var,show t], project 1 lv args body]
    where
        project i v ns body = apply (lambda (map (loc . fst) (zip [v..] ns)) (compileExpr body)) (cdr $ compileVar var) 
compileAlt var (SConstCase c body) = sexp [sexp ["eq?", compileVar var, compileConst c], compileExpr body]
compileAlt _ (SDefaultCase body) = sexp ["else", compileExpr body]


compileConst :: Const -> String
compileConst _ = "const"

compileForeign :: FDesc -> FDesc -> [(FDesc, LVar)] -> String
compileForeign _ _ _ = "ffi"

compileOp :: PrimFn -> [LVar] -> String
compileOp _ _ = "op"

-- Output Helpers
--
-- Implementing a Scheme pretty printer is hard, and there are plenty of
-- them out there, so don't bother to do nice output.
sname :: Name -> String
sname n = "|" ++ showCG n ++ "|"

loc i = "v" ++ show i

sexp xs = "(" ++ intercalate " " xs ++ ")" 
defineFun name args body = sexp ["define", sexp (name:args), body] ++ "\n"
call f args = sexp (f:args)

slet n exp body = sexp ["let", sexp [sexp [n, exp]], body]
sset n exp = call "set!" [n, exp]
cond xs = sexp ("cond":xs)
car l = call "car" [l]
cdr l = call "cdr" [l]
lambda args body = sexp ["lambda", sexp args, body]
apply f l = call "apply" [f, l]