(use srfi-18 matchable)

(define-values (nickname port)
  (match (command-line-arguments)
    ((name . port) (values name (string->number (optional port "5055"))))
    (else (error "usage: name [port]"))))

(include "cube-server.scm")
(include "job-util.scm") ;; job-auto-respawn

(warning (conc "started cube-server on http://localhost:" port))

(define dns-sd-unregister! (dns-sd-register nickname port service-type/cube-browser))

(define server-thread (thread-start! (lambda () (start-server port: port))))

(repl)
