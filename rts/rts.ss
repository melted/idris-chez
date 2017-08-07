;; The runtime library for idris-chez

(define (idris-systeminfo n)
    (case n 
        ((0) "chez")
        ((1) "")
        ((2) "")))

(define (idris-substring off len s) 
    (let* ((l (string-length s))
          (b (max 0 off))
          (x (max 0 len))
          (end (min l (+ b x))))
            (substring s b end)))
    
