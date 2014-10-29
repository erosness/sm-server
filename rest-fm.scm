(use test restlib clojurian-syntax dab-i2c looper)
(import rest notify turi)

;; ==================== fm (catalog) notifications ====================
(define *catalog-notify-connections* '())

(define-handler /v1/catalog/notify
  (make-notify-handler (getter-with-setter
                        (lambda () *catalog-notify-connections*)
                        (lambda (new) (set! *catalog-notify-connections* new)))))

;; do a `curl localhost:5060/v1/catalog/notify` and check with
;; (send-notification "foobar" '((yo . "yo")) (getter-with-setter
;;                                  (lambda () *catalog-notify-connections*)
;;                                  (lambda (new) (set! *catalog-notify-connections* new))))


(define (call-when-modified data-thunk thunk #!optional (pred (constantly #t)))
  (define last #f)
  (define (set-last! x) (set! last x))
  (lambda ()
    ;; in case operation is expensive/noisy a predicate can be
    ;; guard the check with a predicate
    (if (pred)
        ;; wrap in list so we can tell the difference between never ran
        ;; and data-thunk returning #f.
        (let ((check (list (data-thunk))))
          (if (and last (equal? check last))
              #t ;; unmodified, keep looping
              (begin (set-last! check)
                     (thunk))))
        ;; keep going if pred is false
        #t)))


(test
 "call-when-modified (rest-fm)"
 "xx"
 (with-input-from-string "aaaaaaaa"
   ;; should be run twice: once the first time, then once again on
   ;; eof.
   (lambda () (->> (call-when-modified read-char (lambda () (display "x")))
              (loop/count 1000)
              (loop)
              (with-output-to-string)))))


(define (turi-command params)
  (or (and-let* ((hz (alist-ref 'hz params))
                 (val (string->number hz)))
        (ensure-fm-on)
        (fm-frequency val)
        ;; TODO: find IP so zones can reach DAB
        `((url . "http://127.0.0.1:8090/dab/hi")))
      (error "invalid fm params. expected hz key with number value")))

(define-turi-adapter fmfreq->turi "fm"
  turi-command)

(define (fm-tunestatus)
  (symbol->string (parse-dab.tuneStatus (dab-command (fm.tuneStatus)))))

(define (fm-get-state)
  (let* ((freq (fm-frequency))
         (turi-alist `((hz . ,freq))))
   `((title . ,freq)
     (tuneStatus . ,(fm-tunestatus))
     (subtitle . ,(or (fm-radio-text) ""))
     (turi . ,(fmfreq->turi turi-alist))
     (signalStrength . ,(fm-signal-strength)))))


;; poll-based update mechanism. sends a notification whenever
;; fm-get-state has changed. see loop/interval below for update
;; frequency.
(define fm-thread-iteration
  (call-when-modified
   fm-get-state
   (lambda ()
     (send-notification "/v1/catalog/fm/seek"
                        ;; TODO: turi host always resolves to localhost
                        ;; here, since we don't have a current request
                        ;; to look at. Remove it to avoid confusion.
                        (alist-delete 'turi (fm-get-state))
                        (getter-with-setter (lambda () *catalog-notify-connections*)
                                            (lambda (new) (set! *catalog-notify-connections* new)))))
   ;; only query fm frequency if we have listeners
   (lambda () (pair? *catalog-notify-connections*))))

(begin
  (handle-exceptions e (void) (thread-terminate! fm-thread))
  (define fm-thread
    (thread-start!
     (make-thread
      (->> (lambda () (fm-thread-iteration))
           (loop/interval 0.5)
           (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                                ,(condition->list e)))
                               #t))
           (loop))
      "fm-thread"))))
;;(thread-terminate! fm-thread)

(define (ensure-fm-on)
  (if (not (fm-on?)) (fm-turn-on)))

(define fm-step-size 100)

(define-handler /v1/catalog/fm/seek
  (argumentize (lambda (hz)
                 (ensure-fm-on)
                 (cond
                  ((eq? hz #t)
                   (fm-get-state))

                  ((equal? "up" hz)
                   (dab-command (fm.search 'up))
                   `((status . "ok")))

                  ((equal? "down" hz)
                   (dab-command (fm.search 'down))
                   `((status . "ok")))

                  ((string->number hz) =>
                   (lambda (hz)
                     (fm-frequency hz)
                     `((status . "ok"))))))
               '(hz #t)))

