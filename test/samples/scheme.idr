module Main

import Chez

display : String -> SIO ()
display s = foreign FFI_S "display" (String -> SIO ()) s

main : SIO ()
main = display "O RLY?"
