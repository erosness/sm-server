(use posix srfi-18 bitstring
     dab i2c uart slip
     )

(include "dab-init.uart.scm")
;; (include "dab-init.i2c.scm")

;; parameters are thread-local. (current-frame)
(define current-frame (make-parameter 1))

(define (dab-send frameless-msg)
  (let ((response (send-dab-packet ($frame (current-frame) frameless-msg))))
    (current-frame (add1 (current-frame)))
    response))

;; (dab-send (fm.state #f))
(dab-send (dab.state #t))

(dab-send (dab.scan.state #t))
(dab-send (dab.udls))
(dab-send (dab.tune.status))
(dab-send (dab.station 14))
(dab-send (misc.clock.localTime))
(dab-send (audio.sampleRate))


(dab-send (fm.state #t))
(dab-send (fm.frequency))
(dab-send (fm.tuneStatus))

(dab-send (fm.frequency #t))


;; ==============================
;; repl helper that prints incoming frames:
;; (thread-terminate! thread)
(define thread
  (thread-start!
   (lambda ()
     (let loop ()
       (thread-wait-for-i/o! dab-fd #:input)
       (pp (read-dab-packet))
       (thread-sleep! 0.1)
       (loop)))))

