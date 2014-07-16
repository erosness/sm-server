;; minimum includes for cspeaker to work
(use pefat)
(use spiffy matchable medea restlib)


;; launch a main program
(define-values (nickname port)
  (match (command-line-arguments)
    ((name . port) (values name (string->number (optional port "5066"))))
    (else (error "usage: name [port]"))))

;; bind dynamic paramter for everybody to use.
(rest-server-port port)

;; does not contain deps (it's included from cube-server.scm too)
(include "cspeaker.scm")
(import store)

(print "started cspeaker on http://localhost:" port)

(import rest)
;; (define dns-sd-unregister! (register-pq-with-icon-store nickname port))
(define server-thread (start-rest-server!))

(repl*)
