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
(define txt-record (string->symbol (conc "dns-" nickname "-" port)))
(define dns-sd-unregister! (dns-sd-register nickname port service-type/cube-pq txt-record))
(define server-thread (start-rest-server! port))

;; ==================== player name and icon ====================
(define-handler /v1/player
  (lambda ()
    (define player-store (make-store txt-record))
    (if (current-json)
          (begin
            (player-store (current-json))
            (dns-sd-unregister!)
            (set! dns-sd-unregister!
                  (dns-sd-register nickname port service-type/cube-pq txt-record))))
    (or (player-store) '())))

(repl*)
