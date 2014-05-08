(use srfi-18 matchable)

(define-values (nickname port)
  (match (command-line-arguments)
    ((name . port) (values name (string->number (optional port "5055"))))
    (else (error "usage: name [port]"))))

(include "cube-server.scm")

(print "started cube-server on http://localhost:" port)

(import rest)
(define dns-sd-unregister!/browser (dns-sd-register nickname port service-type/cube-browser))
(define dns-sd-unregister!/pq      (dns-sd-register nickname port service-type/cube-pq))
(define server-thread (start-rest-server! port))

(repl)
