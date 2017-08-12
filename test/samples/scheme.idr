module Main

import Chez

display : String -> SIO ()
display s = foreign FFI_S "display" (String -> SIO ()) s

displayBool : Bool -> SIO ()
displayBool b = foreign FFI_S "display" (Bool -> SIO ()) b

schemeEqual : Int -> Int -> SIO Bool
schemeEqual x y = foreign FFI_S "=" (Int -> Int -> SIO Bool) x y

main : SIO ()
main = do
    display "O RLY?"
    displayBool True
    a <-  schemeEqual 1 2
    displayBool a