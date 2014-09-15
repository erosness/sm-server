(module dab-i2c *
(import extras chicken scheme data-structures)

(use dab srfi-18 bitstring dab i2c posix srfi-13
     srfi-14 ;; char-set
     matchable
     )

(define current-dab-fd
  (set-finalizer! (let ((fd (i2c-open "/dev/i2c-2" #x75)))
                    (lambda () fd))
                  (lambda (get-fd) (file-close (get-fd)))))

;; Send a raw dab packet. Add 2-byte "length" field, see FSAPI
;; protocol reference API section 4.3.3 on SCB framing
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

;; if we forget to read responses after sending commands, we'll have
;; unexpected responses buffered. this should clear those. but it
;; should never be necessary!
(define (dab-flush-input)
  (let loop ()
    (if (string-null? (dab-read-packet))
        (void)
        (loop))))

;; synchronous version of dab-send-packet and friends. should be
;; thread-safe too (important because of dab-refresh-channels!)
(define dab-command
  (let ()
    (define mutex (make-mutex 'dab-command))
    (lambda (bs)
      (handle-exceptions e
        (raise e)
        (dynamic-wind
          (lambda () (mutex-lock! mutex))
          (lambda ()
            (define fid-sent (current-frame))
            (dab-flush-input)
            (dab-send bs)
            (let loop ()
              ;; try to not block other srfi-13 threads
              (thread-wait-for-i/o! (current-dab-fd))
              (let ((response (dab-read-packet)))
                (if (string-null? response)
                    (loop)
                    (let ((frame (parse-frame response)))
                      (match frame
                        (('frame fid reply)
                         (if (not (= fid fid-sent))
                             (error (conc "invalid reply frame for fid " fid-sent)
                                    frame))
                         reply)
                        (else (error "invalid DAB frame" frame))))))))
          (lambda () (mutex-unlock! mutex)))))))

;; looks ok:
;; (pp (map thread-join! (list-tabulate 10 (lambda (idx) (thread-start! (lambda () (thread-yield!) (dab-command (dab.tuneStatus))))))))


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
