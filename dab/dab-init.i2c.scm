(use i2c posix bitstring)

(define dab-fd (i2c-open "/dev/i2c-0" #x75))

;; Add 2-byte "length" field, see FSAPI protocol reference API section
;; 4.3.3 on SCB framing
(define (send-dab-packet bs)
  (let* ((len (/ (bitstring-length bs) 8))
         (zbs (bitconstruct (len 16) (bs bitstring))))
    (file-write dab-fd (bitstring->string zbs))))

(define (dab-read-packet)
  (let* ((p (car (file-read dab-fd 512)))
         (len (bitmatch p ( ((l 16) (rest bitstring)) l))))
    ;; remove i2c packet length header:
    (substring p 2 (+ 2 len))))


