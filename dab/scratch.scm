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
(dab-send (fm.frequency #:notify #t))
(dab-send (fm.frequency 87500))
(dab-send (fm.frequency 97500))

(dab-send (fm.tuneStatus))

(dab-send (fm.frequency 100000))
(dab-send (fm.search))
(dab-send (fm.search 'down))
(dab-send (fm.search 'up))

(dab-send (fm.signalStrength))
(dab-send (fm.rds.active))

(dab-send (fm.rds.ps))
(dab-send (fm.rds.pty))
(dab-send (fm.rds.radioText))
(dab-send (fm.rds.radioText #:notify #t))

(dab-send (fm.searchLevel))

;; (thread-sleep! 1)
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

