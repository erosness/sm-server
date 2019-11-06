(use pefat)
(require-extension utf8)
(require-extension utf8-srfi-13)
(require-extension utf8-srfi-14)

(include "store.scm")
(include "notify.scm")
(include "rest.scm")

(use nrepl posix srfi-18 spiffy)

;; provide a repl on our network
(define (start-nrepl #!optional (port (+ (server-port) 1)))
  (thread-start! (lambda () (nrepl port))))


(include "rest-info.scm")
(include "rest-doorbell-out.scm")
