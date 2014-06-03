;;; Cube Speaker service
;;;
;;; Defines REST interface to manipulate and play sound from a
;;; playqueue.
;;;
(use pefat)
(include "incubator.scm")

(include "closing-http-client.scm")

(include "debug-utils.scm")
(include "concurrent-utils.scm")
(include "broadcast.scm")

(include "rest.scm")

(include "process-cli.scm")
(include "player.scm")
(include "playqueue.scm")

(include "dns-sd.scm")

;; rest plugin layers
(include "rest-audio.scm")
(include "rest-player.scm")

(use parley)
;; parley-based repl
(define (repl*)
  (parameterize ((current-input-port (make-parley-port (current-input-port))))
    (repl)))
