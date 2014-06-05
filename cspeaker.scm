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
(include "notify.scm")

(include "rest.scm")

(include "process-cli.scm")
(include "player.scm")
(include "playqueue.scm")

(include "dns-sd.scm")

;; rest plugin layers
(include "rest-audio.scm")
(include "rest-player.scm")

(use nrepl restlib)
(define (repl*) (nrepl (+ *server-port* 1)))
