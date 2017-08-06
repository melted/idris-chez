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
compileConst (I i) = show i
compileConst (BI bi) = show bi
compileConst (Fl d) = show d
compileConst (Ch c) = "#\\"++[c]
compileConst (Str s) = show s
compileConst (B8 w) = show w
compileConst (B16 w) = show w
compileConst (B32 w) = show w
compileConst (B64 w) = show w
-- type const, won't be used, hopefully
compileConst t | isTypeConst t = "#f"
compileConst x = error $ "Unimplemented const " ++ show x

compileForeign :: FDesc -> FDesc -> [(FDesc, LVar)] -> String
compileForeign _ _ _ = "ffi"

compileOp :: PrimFn -> [LVar] -> String
compileOp (LPlus _) xs = op "+" xs
compileOp (LMinus _) xs = op "-" xs
compileOp (LTimes _) xs = op "*" xs
compileOp (LUDiv _) xs = op "quotient" xs
compileOp (LSDiv _) xs = op "/" xs
compileOp (LURem _) xs = op "remainder" xs
compileOp (LSRem _) xs = op "remainder" xs
compileOp (LAnd _) xs = op "bitwise-and" xs
compileOp (LOr _) xs = op "bitwise-ior" xs
compileOp (LXOr _) xs = op "bitwise-xor" xs
compileOp (LCompl ITBig) xs = op "bitwise-not" xs
compileOp (LCompl ty) [x] = call "bitwise-xor" [compileVar x, full ty]
compileOp (LSHL ty) [x, y] = op "bitwise-arithmetic-shift-left" [makeUnsigned ty x, y]
compileOp (LLSHR ty) [x, y] = op "bitwise-arithmetic-shift-right" [makeUnsigned ty x, y]
compileOp (LASHR ty) xs = op "bitwise-arithmetic-shift-right" xs
compileOp (LEq _) xs = op "=" xs
compileOp (LLt ty) xs = op "<" (map (makeUnsigned ty) xs)
compileOp (LLe ty) xs = op "<=" (map (makeUnsigned ty) xs)
compileOp (LGt ty) xs = op ">" (map (makeUnsigned ty) xs)
compileOp (LGe ty) xs = op ">=" (map (makeUnsigned ty) xs)
compileOp (LSLt _) xs = op "<" xs
compileOp (LSLe _) xs = op "<=" xs
compileOp (LSGt _) xs = op ">" xs
compileOp (LSGe _) xs = op ">=" xs
compileOp (LSExt _ _) [x] = compileVar x
compileOp (LZExt ty _) [x] = compileVar $ makeUnsigned ty x
compileOp (LTrunc from to) [x] = call "bitwise-and" [compileVar x, full to]
compileOp LStrConcat xs =  op "string-append" xs
compileOp LStrLt xs = op "string<?" xs
compileOp LStrEq xs = op "string=?" xs
compileOp LStrLen xs =  op "string-length" xs
compileOp (LIntFloat ty) [x] = compileVar x
compileOp (LFloatInt ty) xs = op "floor" xs
compileOp (LIntStr _) xs = op "number->string" xs
compileOp (LStrInt _) xs = op "string->number" xs
compileOp LFloatStr xs = op "number->string" xs
compileOp LStrFloat xs = op "string->number" xs
compileOp (LChInt _) xs = op "char->integer" xs
compileOp (LIntCh _) xs = op "integer->char" xs
compileOp (LBitCast _ _) [x] = compileVar x
compileOp LFExp xs = op "exp" xs 
compileOp LFLog xs = op "log" xs
compileOp LFSin xs = op "sin" xs 
compileOp LFCos xs = op "cos" xs
compileOp LFTan xs = op "tan" xs 
compileOp LFASin xs = op "asin" xs 
compileOp LFACos xs = op "acos" xs 
compileOp LFATan xs = op "atan" xs
compileOp LFSqrt xs = op "sqrt" xs 
compileOp LFFloor xs = op "floor" xs 
compileOp LFCeil xs = op "ceiling" xs 
compileOp LFNegate xs = op "-" xs
compileOp LStrHead [x] = call "string-ref" [compileVar x, "0"] 
compileOp LStrTail [x] = call "substring" [compileVar x, "1", call "string-length" [compileVar x]] 
compileOp LStrCons [c, x] = call "string-append" [call "string" [compileVar c], compileVar x] 
compileOp LStrIndex xs = op "string-ref" xs 
compileOp LStrRev [x] = call "list->string" [call "reverse" [call "string->list" [compileVar x]]] 
compileOp LStrSubstr [off, l, x] = call "substring" [compileVar x, compileVar off, call "+" [compileVar off, compileVar l]]
compileOp LReadStr [_] = call "get-line" [sexp ["current-input-port"]] 
compileOp LWriteStr [_, x] = call "put-string" [sexp ["current-output-port"], compileVar x]
compileOp LSystemInfo [x] = call "idris-systeminfo" [compileVar x] 
compileOp LFork xs = ignore
compileOp LPar xs = ignore
compileOp LCrash [x] = call "error" ["idris", compileVar x]
compileOp LNoOp _ = ""
compileOp (LExternal n) xs = externalOp n xs 

compileOp _ _ = "op"

ignore = ""

full (ITFixed IT8) = "#xff"
full (ITFixed IT16) = "#xffff"
full (ITFixed IT32) = "#xffffffff"
full (ITFixed IT64) = "#xffffffffffffffff"
full ITNative = "#xffffffffffffffff"
full ITChar = "#xffffffff"

op f args = call f (compileVars args) 

externalOp :: Name -> [LVar] -> String
externalOp _ _ = "Ooops"

-- Convert negative numbers to two-complements positive
-- TODO: fill in
makeUnsigned ty x = x

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