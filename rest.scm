(module rest (*uris*
              set-handler!
              define-handler)

(import chicken scheme data-structures player)
(use srfi-69 test uri-common)

(define *uris* (make-hash-table))

(define (set-handler! url thunk)
  (assert (string? url))
  (hash-table-set! *uris* url thunk))

(define-syntax define-handler
  (syntax-rules ()
    ((define-handler path body ...)
     (begin
       (define path body ...)
       (set-handler! (symbol->string 'path) path)))))
)
