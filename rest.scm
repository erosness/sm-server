(module rest (*uris*
              set-handler!
              wrap-changes
              define-handler)

(import chicken scheme data-structures player broadcast)
(use srfi-69 ports test uri-common medea multicast)

;; ==================== handler ====================
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

;; ==================== rest combinators ====================

;; returns a procedure which announces it was been called using UDP
;; broadcasts, with the jsonified value of having called proc.
(define ((wrap-changes path proc) #!rest args)
  (let* ((response (apply proc args))
         (json (with-output-to-string (lambda () (write-json response)))))
    (udp-multicast (change-message path json))
    response))

)
