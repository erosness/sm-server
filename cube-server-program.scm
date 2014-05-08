(use srfi-18 matchable)

(define-values (nickname port)
  (match (command-line-arguments)
    ((name . port) (values name (string->number (optional port "5055"))))
    (else (error "usage: name [port]"))))

(include "cube-server.scm")

(print "started cube-server on http://localhost:" port)

(define dns-sd-unregister! (dns-sd-register nickname port service-type/cube-browser))

(import rest)
(define server-thread (start-rest-server! port))

(repl)
