(use posix srfi-18 bitstring
     uart dab slip)

(define dab-fd (uart-open "/dev/ttymxc0"))

(define my-port (make-input-port (lambda ()
                                   (thread-wait-for-i/o! dab-fd #:input)
                                   (string-ref (car (file-read dab-fd 1)) 0))
                                 (lambda () (file-select dab-fd #f 0))
                                 (lambda () (file-close dab-fd))))

;; TODO: fix slip.scm to write packets to ports
;; important! must escape ESC and END bytes in packet body
(define (send-dab-packet packet)
  (file-write dab-fd (conc "\300"
                           (blob->string (bitstring->blob packet))
                           "\300")))


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


