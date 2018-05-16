(cond-expand
 ((and arm) (use dab))
 (else))

(use test restlib clojurian-syntax looper fmt)
(import rest notify turi incubator)

(import rest-player)
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

(define (ensure-fm-on)
  (dab-abort-if-scanning)
  (if (not (fm-on?)) (fm-turn-on)))


(define (turi-command params)
  (or (and-let* ((hz (alist-ref 'hz params))
                 (val (string->number hz)))
        (ensure-fm-on)
        (fm-frequency val)
        ;; TODO: find IP so zones can reach DAB
        `((url . "default:CARD=imxaudiovenice9")))
      (error "invalid fm params. expected hz key with number value")))

(define-local-turi-adapter fmfreq->turi "fm"
  turi-command)

;; (hz-pretty-print 102550) => "102.55Mhz"
(define (hz-pretty-print hz) (fmt #f (num (/ hz 1000) 10 2) "Mhz"))


;; TODO: make all fm/dab apis not throw?
(define (fm-radio-text-safe)
  (handle-exceptions e
    (begin
      (pp (condition->list e))
      #f)
    (fm-radio-text)))

(define (fm-get-state)
  (let* ((freq (fm-frequency))
         (turi-alist `((hz . ,freq))))
    `((title . ,(fm-radio-ps))     
      (type . "fm")
      (frequency . ,freq)
      (tuneStatus . ,(symbol->string (fm-tunestatus)))
      (subtitle . ,(fm-radio-text-safe))
      (turi . ,(fmfreq->turi turi-alist))
      (signalStrength . ,(fm-signal-strength)))))

(define (fm-searching?)
  (eq? (fm-tunestatus) 'idle))

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
      (fm-pq 1000)
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

		     (fm-pq hz)
                     `((status . "ok"))))))
               '(hz #t)))


