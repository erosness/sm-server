(use pefat)
(use srfi-18 matchable restlib)


;; FIX for http-client error when doing wimp-login
;; openssl needs chicken-syntax which is not included by default.
;; chicken-syntax is also special since it cant be included inside a
;; module but has to be added at toplevel of the binary we're
;; producing.
;; TODO: chicken-syntax is included by default from 4.9.0, remove this
;; after we've upgraded
(use chicken-syntax)

(define-values (nickname port)
  (match (command-line-arguments)
    ((name . port) (values name (string->number (optional port "5055"))))
    (else (error "usage: name [port]"))))

;; bind our server port "globally" for everybody to use
(rest-server-port port)

(include "cube-server.scm")

(print "started cube-server on http://localhost:" port)

(import rest)
(define dns-sd-unregister!/browser (dns-sd-register nickname port service-type/cube-browser))
(define dns-sd-unregister!/pq      (register-pq-with-icon-store nickname port))

(define server-thread (start-rest-server!))

(repl*)
