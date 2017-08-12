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
    (when (port? p) (close-port p)))

(define (idris-chez-get-line p)
    (if (port? p) (get-line p) ""))

(define (idris-chez-get-n n p)
    (if (port? p) (get-string-n p n) ""))

(define (idris-chez-putstring p s)
    (if (port? p) (put-string p s) void)
    0)

;; Chez scheme passes unsigned types as signed to foreign functions
;; but returns them as 0..2^n. Keep them as unsigned for simplicity
;; but we must call these functions before using them.
(define (ui bw)
    (let* ((half (expt 2 (- bw 1)))
          (full (* 2 half)))
        (lambda (v)
            (if (> v half)
                (- v full)
                v))))

;; for a chez ftype
(define (uit ty) (ui (* 8 (foreign-sizeof ty))))

(define ui8 (ui 8))
(define ui16 (ui 16))
(define ui32 (ui 32))
(define ui64 (ui 64))
(define ui-ptr (uit 'void*))

;; disable buffering for stdin and stdout
;; using chez specific forms of the io functions
(define (idris-chez-disable-buffering)
    (current-input-port (standard-input-port 'none (make-transcoder (utf-8-codec))))
    (current-output-port (standard-output-port 'none (make-transcoder (utf-8-codec)))))

(define (idris-chez-stringbuilder)
    (let ((xs '()))
        (case-lambda 
            (() (apply string-append (reverse xs)))
            ((a) (set! xs (cons a xs))))))

;; TODO: memoize foreign functions (that goes for compileForeign too)
(define (idris-chez-memset ptr off val size)
    ((foreign-procedure "memset" (void* int size_t) void*) (ui-ptr (+ ptr off)) val size)
    void)

(define (idris-chez-memmove src srcoff dst dstoff size)
    ((foreign-procedure "memmove" (void* void* size_t) void*) (ui-ptr (+ src srcoff)) (ui-ptr (+ dst dstoff)) size)
    void)

(define (idris-chez-loadlib lib)
    (guard
        (x (else void)) ;; Accept failure to load a lib
        (load-shared-object lib)))

(define (idris-chez-init libs)
    (let* ((mt (symbol->string (machine-type)))
            (l (string-length mt))
            (pf (substring mt (- l 2) l))
            (clib (case pf
                (("nt") "msvcrt")
                (else "libc"))))
        (idris-chez-loadlib clib)
        (map idris-chez-loadlib libs)))

