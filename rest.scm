(module rest (*uris*
              set-handler!
              wrap-changes
              define-handler
              json-handler
              log-handler
              log?
              with-request
              current-host
              start-rest-server!)

(import chicken scheme data-structures broadcast)
(use srfi-18 srfi-69 ports
     test uri-common medea multicast spiffy intarweb
     restlib clojurian-syntax matchable)

(define *server-port* #f)
;; ==================== handler ====================
(define *uris* (make-hash-table))

(define (set-handler! url thunk)
  (assert (string? url))
  (hash-table-set! *uris* url thunk))

(define (find-accessor uri #!optional (uris *uris*))
  (hash-table-ref/default uris uri #f))



(define log? #f)
(define (log-handler thunk)
  (lambda () (if log? (print ";; request: " (uri->string (request-uri (current-request)))))
     (thunk)))

(define (json-handler)
  (let ((uri (uri->string (make-uri path: (uri-path (request-uri (current-request)))))))
    (let ((handler (find-accessor uri)))
      (if handler
          (handler)
          (begin
            (print ";; invalid url visit:" (uri->string (request-uri (current-request))))
            `((error       . ,(conc "not found: " uri))
              (valid-urls  . ,(list->vector (hash-table-keys *uris*)))))))))

;; (define-handler /path (lambda () #f)) now defaults to /v1/path on the
;; interface. this will be a lot of fun to maintain in the long run.
(define-syntax define-handler
  (ir-macro-transformer
   (lambda (x e t)
     (let ((path (e (cadr x)))
           (body (caddr x))
           (rest (cdddr x)))
       (if (pair? rest) (error "illegal define-handler" x))
       (if (not (equal? (substring (conc (symbol->string path) "   ") 0 4)
                        "/v1/"))
           (error "path must start with /v1/" path))
       `(begin
          (define ,path ,body)
          (set-handler! ,(symbol->string path) ,path))))))

;; (define-handler tst identity)
;; (define-handler /v1/tst identity)
;; (define-handler a b c)


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
(define ((wrap-changes path proc
                       #!optional
                       (send-message?
                        (lambda () (not (eq? 'GET (request-method (current-request)))))))
         #!rest args)
  (let* ((response (apply proc args))
         (json (with-output-to-string (lambda () (write-json response)))))
    (if (send-message?) (udp-multicast (change-message path json)))
    response))


;; ==================== util ====================

;; try to find out where request came from (the client url that got it
;; here). this is surprisingly tricky. using host header field and
;; cleverly composing defaults.
(define (request-origin request)

  (define (current-port) (uri-port (request-uri request)))

  (match (and request (header-value 'host (request-headers request)))
    ((host . port) (update-uri (request-uri request)
                               host: host
                               port: (or port (current-port)
                                         *server-port* ;;<-- only applicable in repl
                                         )))
    ;; put together some sensible results when request is #f (ie
    ;; called from repl)
    (else (make-uri host: "localhost"
                    port: *server-port*
                    scheme: 'http
                    query: '()))))

(test-group
 "request-origin"
 (define (str->origin str)
   (->> str
        (open-input-string)
        (read-request)
        (request-origin)
        ((lambda (origin) (and origin (uri->string origin))))))

 ;; note: http scheme is missing. spiffy adds these on real
 ;; requests, though.
 (test "host + port in header"
       "//domain:111/"
       (str->origin "GET / HTTP/1.1\r\nHost: domain:111"))
 (test "host in header"
       "//domain/"
       (str->origin "GET / HTTP/1.1\r\nHost: domain"))

 (test "no host in header"
       "http://localhost"
       (str->origin "GET / HTTP/1.1\r\n")))


;; pick our current hostname (look up request's host header), or
;; fallback to localhost (for debugging .. may make it hard to spot
;; bugs?)
(define (current-host)
  (update-uri (request-origin (current-request))
              path: '(/ "")))

(test-group
 "current-host"

 (test "host from header value"
       (uri-reference "//domain.com/")
       (with-request ("/" `((host ("domain.com" . #f))))
                     (current-host)))


 (test "default host"
       (uri-reference "http://localhost/")
       (current-host)))

;; spawns thread!
(define (start-rest-server! port)
  (set! *server-port* port)
  (thread-start!
   (lambda ()
     (define handler (->> (lambda () (json-handler))
                          (wrap-json)
                          (wrap-errors)
                          (log-handler)))

     (vhost-map `((".*" . ,(lambda (continue) (handler)))))
     (start-server port: port))))


)
