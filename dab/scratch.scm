(use posix srfi-18 bitstring
     dab i2c uart slip
     )

(include "dab-init.uart.scm")
;; (include "dab-init.i2c.scm")


(send-dab-packet (dab.state 1 #t))
(send-dab-packet (dab.scan.state 127 #t))
(send-dab-packet (dab.udls 102 #t))
(send-dab-packet (dab.tune.status 66 #t))
(send-dab-packet (dab.station 105 14))

(send-dab-packet (misc.clock.localTime 111))

(send-dab-packet (audio.sampleRate 112))

;; (send-dab-packet ($frame 998 ($item-get "\x02\x10\x0d\x00")))

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

(send-dab-packet (fm.state 776 #f))
(send-dab-packet (fm.frequency 776))

(integer->status-code #x83)
