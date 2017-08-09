-- Common utility functions

module Chez.Util where

import Idris.Core.TT

import IRTS.Lang

import Data.List

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
toChezCType FString = "'utf-8"
toChezCType FPtr = "'void*"
toChezCType FManagedPtr = "'void*"
toChezCType FCData = "'void*"
toChezCType FUnit = "'void*"
toChezCType FAny = "'void*"
toChezCType (FArith ATFloat) = "'double"
toChezCType (FArith (ATInt ITChar)) = "'wchar_t"
toChezCType (FArith (ATInt ITNative)) = "'int"
toChezCType (FArith (ATInt (ITFixed IT8))) = "'unsigned-8"
toChezCType (FArith (ATInt (ITFixed IT16))) = "'unsigned-16"
toChezCType (FArith (ATInt (ITFixed IT32))) = "'unsigned-32"
toChezCType (FArith (ATInt (ITFixed IT64))) = "'unsigned-64"
toChezCType _ = "'void*"
