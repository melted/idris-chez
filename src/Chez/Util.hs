-- Common utility functions

module Chez.Util where

import Idris.Core.TT

import IRTS.Lang

import Data.Char
import Data.List

import Numeric (showHex)


compileVar :: LVar -> String
compileVar (Loc i) = loc i
compileVar (Glob n) = sname n

compileVars = map compileVar

-- Size of numbers
width (ITFixed IT8) = 8
width (ITFixed IT16) = 16
width (ITFixed IT32) = 32
width (ITFixed IT64) = 64
width ITNative = 64
width ITChar = 32

range ty = show $ 2^(width ty)
full ty = show (2^(width ty) - 1)

halfrange ty = show $ 2^(width ty-1)


-- Output Helpers
--
-- Implementing a Scheme pretty printer is hard, and there are plenty of
-- them out there, so don't bother to do nice output.
sname :: Name -> String
sname n = "|" ++ map legalize (showCG n) ++ "|"
    where
        legalize '#' = '\xa4'
        legalize '|' = '\xa6'
        legalize x = x

loc i = "v" ++ show i

sexp xs = "(" ++ intercalate " " xs ++ ")" 
defineFun name args body = sexp ["define", sexp (name:args), body] ++ "\n\n"
call f args = sexp (f:args)

slet n exp body = sexp ["let", sexp [sexp [n, exp]], body]
sset n exp = call "set!" [n, exp]
cond xs = sexp ("cond":xs)
car l = call "car" [l]
cdr l = call "cdr" [l]
lambda args body = sexp ["lambda", sexp args, body]
apply f l = call "apply" [f, l]
quote s = "'" ++ s

-- Scheme predicates return #t and #f and Idris
-- expects 1 and 0. Fix it up.
predicate p = call "if" [p, "1", "0"] 

cmp f args = predicate $ op f args

ucmp ty f args = predicate $ call f (map (makeUnsigned ty . compileVar) args) 

op f args = call f (compileVars args)

charOp o args = call "integer->char" [call o (map charToInt args)]

charShift True o [x, y] = call "integer->char" 
                            [call "and"
                              [call o [charToInt x, compileVar y]], full ITChar]
charShift False o [x, y] = call "integer->char" [call o [charToInt x, compileVar y]]
charToInt x = call "char->integer" [compileVar x]

clamp :: IntTy -> String -> String
clamp ITBig o = o
clamp ty@(ITFixed _) o = call "modulo" [o, range ty]
clamp it o = call "-" [call "modulo" [call "+" [halfrange it, o], range it], halfrange it]

-- Convert negative numbers to two-complements positive
-- TODO: take string instead of LVar
makeUnsigned :: IntTy -> String -> String
-- TODO: ITBig doesn't really make sense here
makeUnsigned ITBig x = x
makeUnsigned ty x = slet "n" x (call "if" [call "negative?" ["n"],
                                call "+" ["n", range ty],"n"])

makeSigned :: IntTy -> String -> String
makeSigned ty o = slet "n" o (call "if" [call ">" ["n", halfrange ty],
                                call "-" ["n", range ty], "n"])

sstr = schemeString 
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
toChezCType FUnit = "void"
toChezCType FAny = "void*"
toChezCType (FArith ATFloat) = "double"
toChezCType (FArith (ATInt ITChar)) = "wchar_t"
toChezCType (FArith (ATInt ITNative)) = "int"
toChezCType (FArith (ATInt (ITFixed IT8))) = "unsigned-8"
toChezCType (FArith (ATInt (ITFixed IT16))) = "unsigned-16"
toChezCType (FArith (ATInt (ITFixed IT32))) = "unsigned-32"
toChezCType (FArith (ATInt (ITFixed IT64))) = "unsigned-64"
toChezCType _ = "void*"

isCType (FCon c) = head (showCG c) == 'C'   
isCType (FApp c x) = head (showCG c) == 'C'

isFunction fd = case toFType fd of
    FFunction -> True
    FFunctionIO -> True
    _ -> False

getSignature :: FDesc -> (String, [String])
getSignature desc = (fst $ rty desc, map fst $ args desc)
    where
        rty (FApp c [_,ty])
            | c == sUN "C_FnBase" = (ffiType ty, ty)
            | c == sUN "C_FnIO" = (ffiType ty, ty)
            | c == sUN "C_FnT" = rty ty
        rty (FApp c [_,_,ty,fn])
            | c == sUN "C_Fn" = rty fn
        rty x = ("", x)
        args (FApp c [_,ty])
            | c == sUN "C_FnBase" = []
            | c == sUN "C_FnIO" = []
            | c == sUN "C_FnT" = args ty
        args (FApp c [_,_,ty,fn])
            | toFType ty == FUnit = []
            | c == sUN "C_Fn" = (ffiType ty, ty) : args fn
        args _ = []

-- Scheme ffi types
