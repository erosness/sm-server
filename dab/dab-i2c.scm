(module dab-i2c *
(import extras chicken scheme)

(use dab srfi-18 bitstring dab i2c posix)

(define current-dab-fd
  (set-finalizer! (let ((fd (i2c-open "/dev/i2c-2" #x75)))
                    (lambda () fd))
                  (lambda (get-fd) (file-close (get-fd)))))

;; Add 2-byte "length" field, see FSAPI protocol reference API section
;; 4.3.3 on SCB framing
(define (dab-send-packet bs)
  (let* ((len (/ (bitstring-length bs) 8))
         (zbs (bitconstruct (len 16) (bs bitstring))))
    (file-write (current-dab-fd) (bitstring->string zbs))))

(define (dab-read-packet)
  (let* ((p (car (file-read (current-dab-fd) 512)))
         (len (bitmatch p ( ((l 16) (rest bitstring)) l))))
    ;; remove i2c packet length header:
    (substring p 2 (+ 2 len))))


;; parameters are thread-local. (current-frame)
(define current-frame (make-parameter 1))

(define (dab-send frameless-msg)
  (let ((response (dab-send-packet ($frame (current-frame) frameless-msg))))
    (current-frame (add1 (current-frame)))
    response))


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



;; before any module can be turned on (eg dab.state), all the other
;; must be off. only one can be on at the same time.
(define (dab-reset)
  (dab-send (dab.state 'off))
  (dab-send (fm.state 'off))
  (dab-send (audio.buzzer.state 'off)))

(define (dab-turn-on)
  (dab-reset)
  (dab-send (dab.state 'on))
  (dab-send (dab.scan.state 'scan)))


)
