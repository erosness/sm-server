

(define dab-fd (uart-open "/dev/ttymxc0"))


(define my-port (make-input-port (lambda ()
                                   (thread-wait-for-i/o! dab-fd #:input)
                                   (string-ref (car (file-read dab-fd 1)) 0))
                                 (lambda () (file-select dab-fd #f 0))
                                 (lambda () (file-close dab-fd))))

(define (send-dab-packet packet)
  (file-write
   dab-fd
   (with-output-to-string
     (lambda () (slip-write
            ;; add magic checksum
            (conc (bitstring->string packet) "\xef"))))))

(define (read-dab-packet)
  ;; make sure we don't block:
  (and (file-select dab-fd #f 0)
       ;; remove crc checksum:
       (parse-frame (string-drop-right (slip-read my-port) 1))))
