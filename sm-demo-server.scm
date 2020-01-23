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

(include "gpio.scm")
(include "led-matrix.scm")

(include "rest-info.scm")
(include "rest-zeroconf.scm")

(import sm-config)

(map
  (lambda (cap)
    (match cap
      ("doorbell-out" (include "rest-doorbell-out.scm"))
      ("doorbell-in" (include "rest-doorbell-in.scm"))
      ("display8x8" (include "rest-display.scm"))
      (_              (include "rest-noop.scm"))))
    (capability))
