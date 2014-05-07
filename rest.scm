;;; use define-handler to map a url to a handler. it also exposes the
;;; url as a variable so it's easily accessible within your repl.
;;;
;;; there can be separate uris (hash tables of urls => handlers)
;;; specified with optional args. note that if these clash with
;;; existing top-level variables, they will be written. top-level
;;; variables should therefore be used with care.
(module rest (*uris*
              set-handler!
              get-handler
              wrap-changes
              define-handler
              json-handler)

(import chicken scheme data-structures player broadcast)
(use srfi-69 ports test uri-common medea multicast spiffy intarweb test)

;; ==================== handler ====================

(define (set-handler! uris path thunk)
  (assert (string? path))
  (hash-table-set! uris path thunk))

(define (get-handler uris uri)
  (hash-table-ref/default uris uri #f))


;; default uri hash-table
(define *uris* (make-hash-table))

;; uris: a hash-table of "path" => handler
(define (json-handler #!optional (uris *uris*))
  (let ((path (uri->string (make-uri path: (uri-path (request-uri (current-request)))))))
    (let ((handler (get-handler uris path)))
      (if handler
          (handler)
          `((error       . ,(conc "not found: " path))
            (valid-urls  . ,(list->vector (hash-table-keys *uris*))))))))

(define-syntax define-handler
  (syntax-rules ()

    ((define-handler (path uris) body ...)
     (begin
       (define path body ...)
       (set-handler! uris (symbol->string 'path) path)))

    ((define-handler path body ...)
     (define-handler (path *uris*) body ...))))

;; ==================== rest combinators ====================

;; returns a procedure which announces it was been called using UDP
;; broadcasts, with the jsonified value of having called proc.
(define ((wrap-changes path proc) #!rest args)
  (let* ((response (apply proc args))
         (json (with-output-to-string (lambda () (write-json response)))))
    (udp-multicast (change-message path json))
    response))

(test-group
 "define-handler"
 (let ((ht (make-hash-table)))

   (define-handler (/testing ht) (lambda () 'ok))

   (test 'ok (/testing))
   (test 'ok ((get-handler ht "/testing")))))

)
