(use srfi-18 bitstring
     dab slip )

(include "dab-init.uart.scm")
(include "dab-init.i2c.scm")

;; parameters are thread-local. (current-frame)
(define current-frame (make-parameter 1))

(define (dab-send frameless-msg)
  (let ((response (dab-send-packet ($frame (current-frame) frameless-msg))))
    (current-frame (add1 (current-frame)))
    response))

;; (dab-send (fm.state #f))
(dab-send (dab.state #t))

(dab-send (dab.scan.state #t))
(dab-send (dab.udls))
(dab-send (dab.tune.status))
(dab-send (dab.sl.station 7))
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

;; ==============================
;; repl helper that prints incoming frames
(begin
  (handle-exceptions e (void) (thread-terminate! dab-read-thread))
  (define dab-read-thread
    (thread-start!
     (lambda ()
       (let loop ()
         (or
          (let ((packet (dab-read-packet)))
            (if (string-null? packet)
                ;; sleep a little if we didn't get any packets. if we did, try
                ;; reading the next one immediately
                (thread-sleep! 0.1)
                ;; only print if packet isn't empty
                (pp (parse-frame packet)))))
         (loop))))))
