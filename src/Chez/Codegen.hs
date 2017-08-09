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

import Numeric (showHex)
import qualified Data.Text as T

import Chez.Compatibility (fixup, intercept)

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
compileExpr (SError what) = sexp ["error", show "idris", show what]

compileVar :: LVar -> String
compileVar (Loc i) = loc i
compileVar (Glob n) = sname n

compileVars = map compileVar

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

-- Translate a string literal to Scheme format
-- Let's make it easy for us and render everything
-- outside printable ascii stuff as unicode escapes
schemeString :: String -> String
schemeString s = "\"" ++ sift s ++ "\""
    where
        sift "" = ""
        sift ('\\':cs) = "\\\\" ++ sift cs
        sift ('"':cs) = "\\\"" ++ sift cs
        sift (c:cs) | isAscii c && isPrint c = c:sift cs
        sift (c:cs) = "\\x" ++ showHex (ord c) "" ++ ";" ++ sift cs

schemeChar :: Char -> String
schemeChar c = "#\\x" ++ showHex (ord c) "" ++ " "

compileForeign :: FDesc -> FDesc -> [(FDesc, LVar)] -> String
compileForeign rty (FStr name) args = sexp $ [call "foreign-procedure"
                                         [name, sexp (map (ffiType . fst) args), ffiType rty]] ++
                                          map (compileVar . snd) args
compileForeign _ _ _ = "ffi"

compileOp :: PrimFn -> [LVar] -> String
-- char is not like other numeric types in scheme
-- arithmetic on them should be rare, so represent
-- them like scheme chars anyway
compileOp (LPlus (ATInt ITChar)) xs = charOp "+" xs
compileOp (LMinus (ATInt ITChar)) xs = charOp "-" xs
compileOp (LTimes (ATInt ITChar)) xs = charOp "*" xs
compileOp (LUDiv ITChar) xs = charOp "quotient" xs
compileOp (LSDiv (ATInt ITChar)) xs = charOp "/" xs
compileOp (LURem ITChar) xs = charOp "remainder" xs
compileOp (LSRem (ATInt ITChar)) xs = charOp "remainder" xs
compileOp (LAnd ITChar) xs = charOp "bitwise-and" xs
compileOp (LOr ITChar) xs = charOp "bitwise-ior" xs
compileOp (LXOr ITChar) xs = charOp "bitwise-xor" xs
compileOp (LCompl ITChar) [x] = call "integer->char" [call "bitwise-xor" [call "char->integer" [compileVar x], full ITChar]]
-- we don't have to worry about negative chars but we need keep the result in 32 bits
compileOp (LSHL ITChar) xs = charShift True "bitwise-arithmetic-shift-left" xs 
compileOp (LLSHR ITChar) xs = charShift False "bitwise-arithmetic-shift-right" xs
compileOp (LASHR ITChar) xs = charShift False "bitwise-arithmetic-shift-right" xs
compileOp (LEq (ATInt ITChar)) xs = cmp "char=?" xs
compileOp (LLt ITChar) xs = cmp "char<?" xs
compileOp (LLe ITChar) xs = cmp "char<=?" xs
compileOp (LGt ITChar) xs = cmp "char>?" xs
compileOp (LGe ITChar) xs = cmp "char>=?" xs
compileOp (LSLt (ATInt ITChar)) xs = cmp "char<?" xs
compileOp (LSLe (ATInt ITChar)) xs = cmp "char<=?" xs
compileOp (LSGt (ATInt ITChar)) xs = cmp "char>?" xs
compileOp (LSGe (ATInt ITChar)) xs = cmp "char>=?" xs

-- All other numeric types are just a scheme number
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
compileOp (LSHL ty) [x, y] = call "bitwise-arithmetic-shift-left" [makeUnsigned ty x, compileVar y]
compileOp (LLSHR ty) [x, y] = call "bitwise-arithmetic-shift-right" [makeUnsigned ty x, compileVar y]
compileOp (LASHR ty) xs = op "bitwise-arithmetic-shift-right" xs
compileOp (LEq _) xs = cmp "=" xs
compileOp (LLt ty) xs = ucmp ty "<" xs
compileOp (LLe ty) xs = ucmp ty "<=" xs
compileOp (LGt ty) xs = ucmp ty ">" xs
compileOp (LGe ty) xs = ucmp ty ">=" xs
compileOp (LSLt _) xs = cmp "<" xs
compileOp (LSLe _) xs = cmp "<=" xs
compileOp (LSGt _) xs = cmp ">" xs
compileOp (LSGe _) xs = cmp ">=" xs
compileOp (LSExt _ _) [x] = compileVar x
compileOp (LZExt ty _) [x] =  makeUnsigned ty x
compileOp (LTrunc from to) [x] = call "bitwise-and" [compileVar x, full to]
compileOp LStrConcat xs =  op "string-append" xs
compileOp LStrLt xs = cmp "string<?" xs
compileOp LStrEq xs = cmp "string=?" xs
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
compileOp LStrSubstr xs = op "idris-substring" xs
compileOp LReadStr [_] = call "get-line" [sexp ["current-input-port"]] 
compileOp LWriteStr [_, x] = call "put-string" [sexp ["current-output-port"], compileVar x]
compileOp LSystemInfo [x] = call "idris-systeminfo" [compileVar x] 
compileOp LFork [x] = call "fork-thread" [lambda [] (call (sname (sMN 0 "EVAL")) [compileVar x])]
compileOp LPar [x] = compileVar x
compileOp LCrash [x] = call "error" [show "idris", compileVar x]
compileOp LNoOp xs = compileVar (last xs)
compileOp (LExternal n) xs = externalOp n xs 

compileOp _ _ = "op"

width (ITFixed IT8) = 8
width (ITFixed IT16) = 16
width (ITFixed IT32) = 32
width (ITFixed IT64) = 64
width ITNative = 64
width ITChar = 32

range ty = show $ 2^(width ty)
full ty = show (2^(width ty) - 1) 

op f args = call f (compileVars args)

cmp f args = call "if" [op f args, "1", "0"]

ucmp ty f args = call "if" [call f (map (makeUnsigned ty) args), "1", "0"] 

charOp o args = call "integer->char" [call o (map charToInt args)]

charShift True o [x, y] = call "integer->char" 
                            [call "and"
                              [call o [charToInt x, compileVar y]], full ITChar]
charShift False o [x, y] = call "integer->char" [call o [charToInt x, compileVar y]]
charToInt x = call "char->integer" [compileVar x]


-- Convert negative numbers to two-complements positive
makeUnsigned :: IntTy -> LVar -> String
makeUnsigned ITBig x = call "abs" [compileVar x]
makeUnsigned ty x = call "if" [call "negative?" [compileVar x],
                         call "+" [compileVar x, range ty],
                         compileVar x]


externalOp :: Name -> [LVar] -> String
externalOp n [_, x] | n == sUN "prim__readFile" = call "get-string-all" [compileVar x]
externalOp n [_, len, x] | n == sUN "prim__readChars" = call "get-string-n" [compileVar len, compileVar x]
externalOp n [_, x, s] | n == sUN "prim__writeFile" = call "put-string" [compileVar x, compileVar s]
externalOp n [] | n == sUN "prim__stdin" = call "current-input-port" []
externalOp n [] | n == sUN "prim__stdout" = call "current-output-port" []
externalOp n [] | n == sUN "prim__stderr" = call "current-error-port" []
externalOp n [_] | n == sUN "prim__vm" = "'vm" -- just a token, let's elaborate if needed
externalOp n [] | n == sUN "prim__null" = "0"
externalOp n [x, y] | n == sUN "prim__eqPtr" = call "eq?" [compileVar x, compileVar y]
externalOp n [x, y] | n == sUN "prim__eqManagedPtr" = call "eq?" [car (compileVar x), car (compileVar y)]
externalOp n [x] | n == sUN "prim__registerPtr" = compileVar x
externalOp n [_, x, y] | n == sUN "prim__peek8" = call "foreign-ref" ["unsigned-8", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke8" = call "foreign-set!" ["unsigned-8", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peek16" = call "foreign-ref" ["unsigned-16", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke16" = call "foreign-set!" ["unsigned-16", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peek32" = call "foreign-ref" ["unsigned-32", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke32" = call "foreign-set!" ["unsigned-32", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peek64" = call "foreign-ref" ["unsigned-64", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke64" = call "foreign-set!" ["unsigned-64", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peekPtr" = call "foreign-ref" ["void*", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__pokePtr" = call "foreign-set!" ["void*", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peekSingle" = call "foreign-ref" ["float", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__pokeSingle" = call "foreign-set!" ["float", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peekDouble" = call "foreign-ref" ["double", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__pokeDouble" = call "foreign-set!" ["double", compileVar x, compileVar y, compileVar z]
externalOp n [x] | n == sUN "prim__asPtr" = call "car" [compileVar x]
externalOp n [] | n == sUN "prim__sizeofPtr" = call "foreign-sizeof" ["void*"]
externalOp n [x, y] | n == sUN "prim__ptrOffset" = call "+" [compileVar x, compileVar y]

externalOp n _ = call "error" ["idris", "Unimplemented external primitive " ++ show n]

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


-- deconstruct FDesc to FType and, FType to Chez cffi types.

ffiType = toChezCType . toFType

toFType :: FDesc -> FType
toFType (FCon c)
    | c == sUN "C_Str" = FString
    | c == sUN "C_Float" = FArith ATFloat
    | c == sUN "C_Ptr" = FPtr
    | c == sUN "C_MPtr" = FManagedPtr
    | c == sUN "C_CData" = FCData
    | c == sUN "C_Unit" = FUnit
toFType (FApp c [_,ity])
    | c == sUN "C_IntT" = FArith (toAType ity)
    | c == sUN "C_FnT" = toFunType ity
toFType (FApp c [_])
    | c == sUN "C_Any" = FAny
toFType t = FAny

toAType (FCon i)
    | i == sUN "C_IntChar" = ATInt ITChar
    | i == sUN "C_IntNative" = ATInt ITNative
    | i == sUN "C_IntBits8" = ATInt (ITFixed IT8)
    | i == sUN "C_IntBits16" = ATInt (ITFixed IT16)
    | i == sUN "C_IntBits32" = ATInt (ITFixed IT32)
    | i == sUN "C_IntBits64" = ATInt (ITFixed IT64)
toAType t = error (show t ++ " not defined in toAType")

toFunType (FApp c [_,ity])
    | c == sUN "C_FnBase" = FFunction
    | c == sUN "C_FnIO" = FFunctionIO
toFunType (FApp c [_,_,_,ity])
    | c == sUN "C_Fn" = toFunType ity
toFunType _ = FAny

toChezCType :: FType -> String
toChezCType FString = "utf-8"
toChezCType FPtr = "void*"
toChezCType FManagedPtr = "void*"
toChezCType FCData = "void*"
toChezCType FUnit = "void*"
toChezCType FAny = "void*"
toChezCType (FArith ATFloat) = "double"
toChezCType (FArith (ATInt ITChar)) = "wchar_t"
toChezCType (FArith (ATInt ITNative)) = "int"
toChezCType (FArith (ATInt (ITFixed IT8))) = "unsigned-8"
toChezCType (FArith (ATInt (ITFixed IT16))) = "unsigned-16"
toChezCType (FArith (ATInt (ITFixed IT32))) = "unsigned-32"
toChezCType (FArith (ATInt (ITFixed IT64))) = "unsigned-64"
toChezCType _ = "void*"