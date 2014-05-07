(use srfi-18 matchable)

(include "cube-server.scm")
(include "job-util.scm") ;; job-auto-respawn

(define port (match (command-line-arguments)
               ((sport) (string->number sport))
               (else 5055)))
(define hostname (string-trim-right (with-input-from-pipe "hostname" read-string)))

(warning "started cube-server on http://localhost:" port)

(define dns-sd-unregister! (dns-sd-register (conc hostname "-cube") port
                                            service-type/cube-browser))

(define server-thread (thread-start! (lambda () (start-server port: port))))

(repl)
