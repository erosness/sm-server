(use posix uart srfi-18 slip bitstring)

(define dab-fd (uart-open "/dev/ttymxc0"))


(define my-port (make-input-port (lambda ()
                                   (thread-wait-for-i/o! dab-fd #:input)
                                   (string-ref (car (file-read dab-fd 1)) 0))
                                 (lambda () (file-select dab-fd #f 0))
                                 (lambda () (file-close dab-fd))))

(define (dab-send-packet packet)
  (file-write
   dab-fd
   (with-output-to-string
     (lambda () (slip-write
            ;; add magic checksum
            (conc (bitstring->string packet) "\xef"))))))

(define (dab-read-packet)
  ;; make sure we don't block:
  (thread-wait-for-i/o! dab-fd #:input)
  (and (file-select dab-fd #f 0)
       ;; remove crc checksum:
       (string-drop-right (slip-read my-port) 1)))
