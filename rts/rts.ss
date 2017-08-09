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


;; TODO: Add error handling.
(define (idris-chez-fileopen file mode)
        (case mode 
            ("r") (open-file-input-port file (file-options 'no-create))
            ("wx") (open-file-output-port file)
            ("a") (open-file-output-port file (file-options 'no-fail 'no-truncate))
            ("r+") (open-file-input/output-port file (file-options 'no-create))
            ("w+") (open-file-input/output-port file (file-options 'no-fail))
            ("w+x") (open-file-input/output-port file)
            ("a+") (open-file-input/output-port file (file-options 'no-fail 'no-truncate)))))
