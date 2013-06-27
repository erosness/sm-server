(module blobbery *

(import chicken scheme)

;; convenience macro for constructing blobs
;; (why is this not part of core, actually?)
;; note that there is a '#${ 00 ff } blob reader-macro for chicken,
;; but blobs cannot contain comments.
;; use like this: (blob 00 01 0a  10 1a  aa  ff FF)
(define-syntax blob-string
  (er-macro-transformer
   (lambda (x r t)
     (apply string
            (map (lambda (x) (integer->char (string->number (conc x) 16)))
                 (cdr x))))))

(define-syntax blob
  (syntax-rules ()
    ((_ hex ...) (string->blob (blob-string hex ...)))))

)
