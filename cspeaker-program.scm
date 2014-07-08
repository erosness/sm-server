;; minimum includes for cspeaker to work
(use pefat spiffy matchable medea)


;; launch a main program
(define-values (nickname port)
  (match (command-line-arguments)
    ((name . port) (values name (string->number (optional port "5066"))))
    (else (error "usage: name [port]"))))

;; does not contain deps (it's included from cube-server.scm too)
(include "cspeaker.scm")
(import store)

(print "started cspeaker on http://localhost:" port)

(import rest)
;; (define dns-sd-unregister! (register-pq-with-icon-store nickname port))
(define server-thread (start-rest-server! port))

(repl*)
