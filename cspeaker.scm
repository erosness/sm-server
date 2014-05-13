;;; Cube Speaker service
;;;
;;; Defines REST interface to manipulate and play sound from a
;;; playqueue.
;;;
(use pefat)

(include "debug-utils.scm")
(include "concurrent-utils.scm")
(include "broadcast.scm")

(include "rest.scm")

(include "process-cli.scm")
(include "player.scm")
(include "playqueue.scm")

(include "dns-sd.scm")

;; rest plugin layers
(include "rest-pq.scm")

