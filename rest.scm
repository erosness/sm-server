(module rest (wrap-changes
              return-url
              log?
              current-host)

(import chicken scheme data-structures notify)
(use srfi-18 srfi-69 ports
     test uri-common medea spiffy intarweb
     restlib clojurian-syntax matchable)

;; ==================== handler ====================


;; (define-handler tst identity)
;; (define-handler /v1/tst identity)
;; (define-handler a b c)


;; simple helper to keep track of where we produce URLs that point
;; back to ourselves. currently only useful for assertions.
(begin-for-syntax
 (import chicken)
 (use srfi-13)) ;; for compile-time string-prefix?
(define-syntax return-url
  (ir-macro-transformer
   (lambda (x e t)
     (let ((parts (cdr x)))

       (define (check str)
         (if (string-prefix? "/v1" str)
             (error "don't include /v1/ in return urls. they are version-protocol-relative.")))

       (for-each (lambda (x) (and (string? x) (check x))) parts)
       `(conc ,@parts)))))



;; ==================== rest combinators ====================

;; returns a procedure which announces it was been called using UDP
;; broadcasts, with the jsonified value of having called proc.
(define ((wrap-changes path proc
                       #!optional
                       (send-message?
                        (lambda () (not (eq? 'GET (request-method (current-request)))))))
         #!rest args)
  (let* ((response (apply proc args)))
    (if (send-message?) (send-notification path response))
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
                               port: (or port (rest-server-port))))
    ;; put together some sensible results when request is #f (ie
    ;; called from repl)
    (else (make-uri host: "localhost"
                    port: (rest-server-port)
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

 (parameterize ((rest-server-port 80))
  ;; note: http scheme is missing. spiffy adds these on real
  ;; requests, though.
  (test "host + port in header"
        "//domain:111/"
        (str->origin "GET / HTTP/1.1\r\nHost: domain:111"))
  (test "host in header"
        "//domain:80/"
        (str->origin "GET / HTTP/1.1\r\nHost: domain"))

  (test "no host in header"
        "http://localhost"
        (str->origin "GET / HTTP/1.1\r\n"))))


;; pick our current hostname (look up request's host header), or
;; fallback to localhost (for debugging .. may make it hard to spot
;; bugs?)
(define (current-host)
  (update-uri (request-origin (current-request))
              path: '(/ "")
              query: '()))

(test-group
 "current-host"

 (parameterize ((rest-server-port #f))
   (test "host from header value"
         (uri-reference "//domain.com/")
         (with-request ("/" `((host ("domain.com" . #f))))
                       (current-host)))


   (test "default host"
         (uri-reference "http://localhost/")
         (current-host))))



)
