;; The runtime library for idris-chez

(define (idris-systeminfo n)
    (case n 
        ((0) "chez")
        ((1) (symbol->string (machine-type)))
        ((2) "")))

(define (idris-substring off len s) 
    (let* ((l (string-length s))
          (b (max 0 off))
          (x (max 0 len))
          (end (min l (+ b x))))
            (substring s b end)))

(define last-idris-io-error #f)
(define idris-errored-ports '())

(define (idris-chez-isnull p)
    (cond 
        ((number? p) (= p 0))
        (else #f)))

(define (idris-chez-error-handler x)
    (cond 
        ((i/o-read-error? x) (set! last-idris-io-error 1))
        ((i/o-write-error? x) (set! last-idris-io-error 2))
        ((i/o-file-does-not-exist-error? x) (set! last-idris-io-error 3))
        ((i/o-file-protection-error? x) (set! last-idris-io-error 4))
        (else (set! last-idris-io-error 5)))
    0)

(define (idris-chez-fileopen file mode)
    (guard
        ;; exception handler
        (x ((i/o-error? x) (idris-chez-error-handler x)))
        ;; open file
        (idris-chez-open file mode)))


(define (idris-chez-open file mode)
    (define tc (make-transcoder (utf-8-codec)))
    (define bm (buffer-mode line))
    (case mode 
        (("r") (open-file-input-port file (file-options) bm tc))
        (("w") (open-file-output-port file (file-options no-fail) bm tc))
        (("wx") (open-file-output-port file (file-options) bm tc))
        (("a") (open-file-output-port file (file-options no-fail no-truncate) bm tc))
        (("r+") (open-file-input/output-port file (file-options no-create) bm tc))
        (("w+") (open-file-input/output-port file (file-options no-fail) bm tc))
        (("w+x") (open-file-input/output-port file (file-options) bm tc))
        (("a+") (open-file-input/output-port file (file-options no-fail no-truncate) bm tc))
        (else (raise (make-io-error)))))

(define (idris-chez-getfileerror)
    (if (last-idris-io-error)
        (if (< last-idris-io-error 5) (list last-idris-io-error 0) (list 0 1))
        (list 0 0)))

(define (idris-chez-showerror n)
    (case n
        ((0) "No error"))
        ((1) "Generic error"))

(define (idris-chez-fileerror p)
    (member (port-file-descriptor p) idris-errored-ports))


(define (idris-chez-fgetc p)
    (guard (x (else (set! idris-errored-ports (cons (port-file-descriptor p) idris-errored-ports))
                    #\x0))
            (let ((ch (get-char p))) ;; TODO: need to handle eof??
                ch)))

(define (idris-chez-popen cmd mode)
    (guard
        (x ((i/o-error? x) (idris-chez-error-handler x)))
        (case mode
            (("r" "w") (let* ((p (process cmd))
                                (i (car p))
                                (o (cadr p)))
                            (case mode
                                (("r") (close-port o) i)
                                (("w") (close-port i) o)
            (else 0)))))))

(define (idris-chez-close-port p)
    (if (port? p) (close-port p)))

(define (idris-chez-get-all p)
    (if (port? p) (get-string-all p) ""))

(define (idris-chez-get-n n p)
    (if (port? p) (get-string-n p n) ""))

(define (idris-chez-putstring p s)
    (if (port? p) (put-string p s) void)
    0)

(define (idris-chez-stringbuilder)
    (let ((xs '()))
        (case-lambda 
            (() (apply string-append (reverse xs)))
            ((a) (set! xs (cons a xs))))))

(define (idris-chez-init libs)
    (let* ((mt (symbol->string (machine-type)))
           (l (string-length mt))     
           (pf (substring mt (- l 2) l))
           (clib (case pf
             (("nt") "msvcrt")
             (else "libc"))))
             (load-shared-object clib)
             (map load-shared-object libs)))

