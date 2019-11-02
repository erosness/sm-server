(use pefat)
(require-extension utf8)
(require-extension utf8-srfi-13)
(require-extension utf8-srfi-14)

(include "store.scm")
(include "rest.scm")

(use nrepl posix srfi-18 spiffy)

;; provide a repl on our network
(define (start-nrepl #!optional (port (+ (server-port) 1)))
  (thread-start! (lambda () (nrepl port))))

(include "rest-info.scm")
(include "turi.scm")


(include "rest-tone.scm")
(include "rest-alsa-capture.scm")
(include "rest-record-player.scm")
