(use posix srfi-18 bitstring
     dab i2c uart slip
     )

(include "dab-init.uart.scm")
;; (include "dab-init.i2c.scm")

(send-dab-packet (dab-set-state 127 #t))
(send-dab-packet (dab-set-scan-state 200 #t))
(send-dab-packet (dab-set-udls 102 #t))
(send-dab-packet (dab-set-tune-status 66 #t))
(send-dab-packet (dab-set-station 105 14))

(for-each (lambda (i)
            (send-dab-packet (dab-get-uservice (+ 1000 i) i)))
          (iota 50))

;; next available packet or #f (non-blocking)
(if (file-select dab-fd #f 0)
    (parse-frame (slip-read my-port))
    'would-block)

(uart-close dab-fd)

;; read it all: (crashes when you C-c C-c)
;; (let loop () (pp (parse-frame (read-slip my-port))) (loop))


