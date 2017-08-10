module Main

-- Yeah, this is for windows only 
%lib chez "kernel32"

beep : Int -> Int -> IO Int
beep freq dur = foreign FFI_C "Beep" (Int -> Int -> IO Int) freq dur

main : IO ()
main = do
    ok <- beep 1000 1000
    print ok