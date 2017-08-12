module Chez

public export
data SchemeVal = Op1


    
-- TODO: need lists and vector. Should fix representation of them first
public export
data SchemeTypes : Type -> Type where
    S_Int : SchemeTypes Int
    S_Nat : SchemeTypes Nat
    S_Integer : SchemeTypes Integer
    S_Real : SchemeTypes Double
    S_Bool : SchemeTypes Bool
    S_Unit : SchemeTypes ()
    S_Char : SchemeTypes Char
    S_String : SchemeTypes String

    S_Fun : SchemeTypes a -> SchemeTypes b -> SchemeTypes (a -> b)
    S_Opaque : SchemeTypes SchemeVal

public export
FFI_S : FFI
FFI_S = MkFFI SchemeTypes String String

public export
SIO : Type -> Type
SIO = IO' FFI_S