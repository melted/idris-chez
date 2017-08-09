

module Chez.Operators(compileOp, compileVar, compileVars) where

import Idris.Core.TT

import IRTS.Lang

import Chez.Util

compileVar :: LVar -> String
compileVar (Loc i) = loc i
compileVar (Glob n) = sname n

compileVars = map compileVar



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
-- TODO, constrain to correct ranges
-- TODO: Bit manipulation in basic011 has started failing
compileOp (LPlus ATFloat) xs = op "+" xs
compileOp (LPlus (ATInt ITBig)) xs = op "+" xs
compileOp (LPlus (ATInt it)) xs = clamp it (op "+" xs)
compileOp (LMinus ATFloat) xs = op "-" xs
compileOp (LMinus (ATInt ITBig)) xs = op "-" xs
compileOp (LMinus (ATInt it)) xs = clamp it (op "-" xs)
compileOp (LTimes ATFloat) xs = op "*" xs
compileOp (LTimes (ATInt ITBig)) xs = op "*" xs
compileOp (LTimes (ATInt it)) xs = clamp it (op "*" xs)

compileOp (LUDiv _) xs = op "quotient" xs
compileOp (LSDiv ATFloat) xs = op "/" xs
compileOp (LSDiv (ATInt _)) xs = op "quotient" xs

compileOp (LURem _) xs = op "remainder" xs
compileOp (LSRem _) xs = op "remainder" xs
compileOp (LAnd _) xs = op "bitwise-and" xs
compileOp (LOr _) xs = op "bitwise-ior" xs
compileOp (LXOr _) xs = op "bitwise-xor" xs
compileOp (LCompl ITBig) xs = op "bitwise-not" xs
compileOp (LCompl ty) [x] = call "bitwise-xor" [compileVar x, full ty]
compileOp (LSHL ITBig) xs = op "bitwise-arithmetic-shift-left" xs 
compileOp (LSHL ty@(ITFixed _)) [x, y] = call "and" [call "bitwise-arithmetic-shift-left" 
                                            [compileVar x, compileVar y], full ty]
compileOp (LSHL ty) [x, y] = call "and" [(makeSigned ty (call "bitwise-arithmetic-shift-left" 
                                            [makeUnsigned ty (compileVar x), compileVar y])), full ty]
compileOp (LLSHR ty@(ITFixed _)) xs = op "bitwise-arithmetic-shift-right" xs
compileOp (LLSHR ty) [x, y] = makeSigned ty (call "bitwise-arithmetic-shift-right" [makeUnsigned ty (compileVar x), compileVar y])
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
compileOp (LSExt from@(ITFixed _) to@(ITFixed _)) [x] = makeUnsigned to $ makeSigned from $ compileVar x
compileOp (LSExt ty@(ITFixed _) _) [x] = makeSigned ty $ compileVar x
compileOp (LSExt _ _) [x] = compileVar x
compileOp (LZExt (ITFixed _) _) [x] = compileVar x
compileOp (LZExt ty _) [x] =  makeUnsigned ty (compileVar x)
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

compileOp op _ = error "Unknown SOp: " ++ show op


op f args = call f (compileVars args)

cmp f args = call "if" [op f args, "1", "0"]

ucmp ty f args = call "if" [call f (map (makeUnsigned ty . compileVar) args), "1", "0"] 

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
externalOp n [x, y] | n == sUN "prim__registerPtr" = compileVar x
externalOp n [_, x, y] | n == sUN "prim__peek8" = call "foreign-ref" ["'unsigned-8", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke8" = call "foreign-set!" ["'unsigned-8", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peek16" = call "foreign-ref" ["'unsigned-16", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke16" = call "foreign-set!" ["'unsigned-16", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peek32" = call "foreign-ref" ["'unsigned-32", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke32" = call "foreign-set!" ["'unsigned-32", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peek64" = call "foreign-ref" ["'unsigned-64", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__poke64" = call "foreign-set!" ["'unsigned-64", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peekPtr" = call "foreign-ref" ["'void*", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__pokePtr" = call "foreign-set!" ["'void*", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peekSingle" = call "foreign-ref" ["'float", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__pokeSingle" = call "foreign-set!" ["'float", compileVar x, compileVar y, compileVar z]
externalOp n [_, x, y] | n == sUN "prim__peekDouble" = call "foreign-ref" ["'double", compileVar x, compileVar y]
externalOp n [_, x, y, z] | n == sUN "prim__pokeDouble" = call "foreign-set!" ["'double", compileVar x, compileVar y, compileVar z]
externalOp n [x] | n == sUN "prim__asPtr" = call "car" [compileVar x]
externalOp n [] | n == sUN "prim__sizeofPtr" = call "foreign-sizeof" ["'void*"]
externalOp n [x, y] | n == sUN "prim__ptrOffset" = call "+" [compileVar x, compileVar y]

externalOp n _ = call "error" [show $ "idris", show $ "Unimplemented external primitive " ++ show n]
