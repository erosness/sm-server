(module rest (*uris*
              set-handler!
              wrap-changes
              define-handler
              json-handler
              with-request
              start-rest-server!)

(import chicken scheme data-structures player broadcast)
(use srfi-18 srfi-69 ports
     test uri-common medea multicast spiffy intarweb
     restlib clojurian-syntax)

;; ==================== handler ====================
(define *uris* (make-hash-table))

(define (set-handler! url thunk)
  (assert (string? url))
  (hash-table-set! *uris* url thunk))

(define (find-accessor uri #!optional (uris *uris*))
  (hash-table-ref/default uris uri #f))

(define (json-handler)
  (let ((uri (uri->string (make-uri path: (uri-path (request-uri (current-request)))))))
    (let ((handler (find-accessor uri)))
      (if handler
          (handler)
          `((error       . ,(conc "not found: " uri))
            (valid-urls  . ,(list->vector (hash-table-keys *uris*))))))))

(define-syntax define-handler
  (syntax-rules ()
    ((define-handler path body)
     (begin
       (define path body)
       (set-handler! (symbol->string 'path) path)))))


;; ==================== test utils ====================

;; run body in the context of the uri path.
(define-syntax with-request
  (syntax-rules ()
    ((_ (uri hdrs) body ...)
     (parameterize ((current-request (make-request uri: (uri-reference uri)
                                                   headers: (headers hdrs))))
       body ...))

    ((_ uri body ...) (with-request (uri '()) body ...))))


;; ==================== rest combinators ====================

;; returns a procedure which announces it was been called using UDP
;; broadcasts, with the jsonified value of having called proc.
(define ((wrap-changes path proc) #!rest args)
  (let* ((response (apply proc args))
         (json (with-output-to-string (lambda () (write-json response)))))
    (udp-multicast (change-message path json))
    response))


;; ==================== util ====================

;; spawns thread!
(define (start-rest-server! port)
  (thread-start!
   (lambda ()
     (define handler (->> (lambda () (json-handler))
                          (wrap-json)
                          (wrap-errors)))

     (vhost-map `((".*" . ,(lambda (continue) (handler)))))
     (start-server port: port))))


)
