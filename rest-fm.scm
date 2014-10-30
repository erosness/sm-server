(use test restlib clojurian-syntax dab-i2c looper)
(import rest notify turi incubator)

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


(define (fm-searching?)
  (equal? (fm-tunestatus) "idle"))

;;; -------- Search
(define fm-search-iteration
  (lambda ()
    ;; Check if we're still searching before sending the
    ;; notifications, this way the last msg we send out will be the
    ;; state _after_ the search stopped
    (let ((searching? (fm-searching?))
          (state (fm-get-state)))
      (send-notification "/v1/catalog/fm/seek"
                         ;; TODO: turi host always resolves to localhost
                         ;; here, since we don't have a current request
                         ;; to look at. Remove it to avoid confusion.
                         (alist-delete 'turi state)
                         (getter-with-setter (lambda () *catalog-notify-connections*)
                                             (lambda (new) (set! *catalog-notify-connections* new))))
      ;; Keep looping?
      searching?)))

(define (notify-fm-search-state interval)
  (define notify-thread (make-thread
                         (->> fm-search-iteration
                              (loop/interval interval)
                              (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                                                   ,(condition->list e)))
                                                  ;; halt on exceptions
                                                  #f))
                              (loop))
                         "fm-notify-thread"))
  (thread-start! notify-thread))

;; Thread safety
(define *fm-notify-thread* (make-atom #f))
(define (fm-notify-alive?)
  (and (thread? (*fm-notify-thread*))
       (not (eq? (thread-state (*fm-notify-thread*))
                 'dead))))

;; Start searching
(define (fm-search-with-notify direction)
  (fm-search direction)
  (if (not (fm-notify-alive?))
      (*fm-notify-thread* (notify-fm-search-state 1))))

(define (ensure-fm-on)
  (if (not (fm-on?)) (fm-turn-on)))

(define-handler /v1/catalog/fm/seek
  (argumentize (lambda (hz)
                 (ensure-fm-on)
                 (cond
                  ((eq? hz #t)
                   (fm-get-state))

                  ((equal? "up" hz)
                   (fm-search-with-notify 'up)
                   `((status . "ok")))

                  ((equal? "down" hz)
                   (fm-search-with-notify 'down)
                   `((status . "ok")))

                  ((string->number hz) =>
                   (lambda (hz)
                     ;; stop any ongoing searches, this also has the
                     ;; nice side-effect of notifying clients that the
                     ;; frequency has changed
                     (fm-search-with-notify 'idle)
                     (fm-frequency hz)

                     `((status . "ok"))))))
               '(hz #t)))

