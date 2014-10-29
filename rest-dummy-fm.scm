(use test restlib clojurian-syntax looper matchable gochan)
(import rest notify turi)

(define *catalog-notify-connections* '())

(define-handler /v1/catalog/notify
  (make-notify-handler (getter-with-setter
                        (lambda () *catalog-notify-connections*)
                        (lambda (new) (set! *catalog-notify-connections* new)))))

(define (turi-command params)
  (or (and-let* ((hz (alist-ref 'hz params))
                 (val (string->number hz)))
        ;; (ensure-fm-on)
        (fm-frequency val)
        ;; TODO: find IP so zones can reach DAB
        `((url . "http://127.0.0.1:8090/dab/hi")))
      (error "invalid fm params. expected hz key with number value")))

(define-turi-adapter fmfreq->turi "fm" turi-command)

(define (gaussian-rand x)
  (let ((rands (map random (list x x x))))
    (/ (reduce + 0 rands) 3)))

(define-syntax define-delayed
  (syntax-rules ()
    ((_ name body ...)
     (begin
       (define name
         (thread-sleep! (/ (gaussian-rand 100) 100))
         body ...)))))

(define mock-freq 89000)
(define (fm-frequency . hz)
  (if (null? hz)
      mock-freq
      (begin
        (set! mock-freq (car hz))
        mock-freq)))

(define (fm-radio-text)
  (match mock-freq
    (99000 "NRK P3")
    (92000 "Foobar")
    (95600 "HOHOHOHO!")
    (89100 "Test channel 2")
    (else "")))

(define (clamp-range min max)
  (lambda (x)
    (cond
     ((<= min x max) x)
     ((< x min) (- max (modulo min x)))
     (else (+ min (modulo x max))))))

(define fm-range (clamp-range 87500 101000))
(define fm-signal-strength -89)
(define tune-status "decoding")

(define (fm-get-state)
  (let* ((freq (fm-frequency))
         (turi-alist `((hz . ,freq))))
   `((title . ,freq)
     (tuneStatus . ,tune-status)
     (subtitle . ,(fm-radio-text))
     (turi . ,(fmfreq->turi turi-alist))
     (signalStrength . ,fm-signal-strength))))

;; delay sending 
(define-delayed (fm-get-state-delayed) (fm-get-state))


(define c (make-gochan))
(define chan-thread
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (and-let* ((msg (gochan-receive c)))
          (send-notification "/v1/catalog/fm/seek"
                             msg
                             (getter-with-setter (lambda () *catalog-notify-connections*)
                                                 (lambda (new) (set! *catalog-notify-connections* new))))
          (loop))
        (print "exiting " (current-thread)))))))

;; (thread-state chan-thread)
;; (thread-terminate! chan-thread)


;; fake search
(define (search direction)
  (thread-start!
   (make-thread
    (lambda ()
      (let ((op (if (eq? direction 'up) + -)))
        (set! tune-status "idle")
        (set! mock-freq (fm-range (op mock-freq 100) ))
        (let loop ()
          (if (not (equal? (fm-radio-text) ""))
              (set! tune-status "decoding")
              (begin
                (set! mock-freq (fm-range (op mock-freq 100) ))
                (gochan-send c (fm-get-state-delayed))
                (loop)))))))))

(define-handler /v1/catalog/fm/seek
  (argumentize (lambda (hz)
                 (cond
                  ((eq? hz #t)
                   (fm-get-state-delayed))

                  ((equal? "up" hz)
                   (search 'up)
                   `((status . "ok")))

                  ((equal? "down" hz)
                   (search 'down)
                   `((status . "ok")))

                  ((equal? "step-up" hz)
                   (let ((freq (fm-frequency)))
                     (fm-frequency (+ freq fm-step-size))
                     (gochan-send c (fm-get-state-delayed))
                     `((status . "ok"))))

                  ((equal? "step-down" hz)
                   (let ((freq (fm-frequency)))
                     (fm-frequency (- freq fm-step-size))
                     (gochan-send c (fm-get-state-delayed))
                     `((status . "ok"))))

                  ((string->number hz) =>
                   (lambda (hz)
                     (fm-frequency hz)
                     (gochan-send c (fm-get-state-delayed))
                     `((status . "ok"))))))
               '(hz #t)))
