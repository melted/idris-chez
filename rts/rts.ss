;; The runtime library for idris-chez

(define (idris-systeminfo n)
    (case n 
        ((0) "chez")
        ((1) "")
        ((2) "")))
