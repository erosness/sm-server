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
    (pp `(DAB send-packet ,(bitstring->blob bs)))
    (file-write (current-dab-fd) (bitstring->string zbs))))

(define (dab-read-packet)
  (let* ((p (car (file-read (current-dab-fd) 512)))
         (len (bitmatch p ( ((l 16) (rest bitstring)) l))))
    ;; remove i2c packet length header:
    (substring p 2 (+ 2 len))))

;; parameters are thread-local. (current-frame)
(define current-frame (make-parameter 1))

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

    ;; send a dab message asynchronously (without frame)
    (define (dab-send frameless-msg)
      (let ((response (dab-send-packet ($frame (current-frame) frameless-msg))))
        (current-frame (add1 (current-frame)))
        response))

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


;; turn off all submodules.
;;
;; before any module can be turned on (eg dab.state), all the other
;; must be off. only one can be on at the same time.
(define (dab-reset)
  (dab-command (dab.state 'off))
  (dab-command (fm.state 'off))
  (dab-command (audio.buzzer.state 'off)))

;; turn off everything, then turn dab back on
(define (dab-turn-on)
  (dab-reset)
  (dab-command (dab.state 'on)))


;; ====================
;; TODO: the following should be part of some dab module and i/o
;; independent.


;; TODO: apply this in all nt:e8 structures instead
(define (parse-dab.scan.state data)
  (match data
    (('item-get-response (FS_OK . string))
     (case (char->integer (string-ref string 0))
       ((0) 'idle)
       ((1) 'scan)
       (else (error "invalid dab.scan.state return code " data))))
    (else (error "invalid dab.scan.state data" data))))


;; TODO: apply this to all $list-get instead
(define (parse-dab.sl.uService data)
  (match data
    (('list-get-response 'FS_OK (channel . rest))
     (string-trim-both channel (char-set #\space #\newline #\nul)))
    (('list-get-response 'FS_FAIL "") #f)
    (else (error "invalid dab.sl.uService response" data))))

(define (parse-dab.tuneStatus data)
  (match data
    (('item-get-response ('FS_OK . str))
     (match str
       ("\x02" 'decoding)
       ("\x01" 'tuned)
       ("\x00" 'idle)
       (else (error "no matching " str))))
    (else (error "no matching" data))))

;; return dab.sl.station as int. TODO: make generic
(define (parse-dab.sl.station #!optional (data (dab-command (dab.sl.station))))
  (match data
    (('item-get-response (FS_OK . str))
     (bitmatch str (((x 32)) x)))))

(define (dab-channel-name idx)
  (parse-dab.sl.uService (dab-command (dab.sl.uService idx))))

;; list all channels (idx . "label"). this is slow.
(define (dab-channels*)
  (let loop ((n 1)
             (res '()))
    (let ((channel (dab-channel-name n)))
      (if channel
          (loop (add1 n)
                (cons (cons n channel) res))
          (reverse res)))))

;; do a full scan. this takes a long time.
(define (dab-full-scan)
  (dab-command (dab.scan.state 'scan))
  (let loop ()
    (print ";; DAB is scanning ...")
    (case (parse-dab.scan.state (dab-command (dab.scan.state)))
      ((scan)
       (thread-sleep! 1)
       (loop))
      (else (void)))))

(define *dab-channels* '())
(define (dab-refresh-channels!)
  (dab-full-scan)
  ;; TODO: thread safety
  (set! *dab-channels* (dab-channels*)))

(define (dab-channels) *dab-channels*)


)
