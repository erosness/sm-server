(use test restlib looper clojurian-syntax)

(include "amixer.scm")

(import rest notify amixer)

(define mute      #f)
(define volume    50)
(define equalizer '#(50 50 50 50 50))

(define-syntax define-simple-wrapper
  (syntax-rules ()
    ((_ path variable predicate)
     (define-handler path
       (wrap-changes (symbol->string 'path)
                     (lambda ()
                       (if (current-json)
                           (let ((value (alist-ref 'value (current-json))))
                             (if (predicate value)
                                 (set! variable value)
                                 (error (conc "illegal value for " 'path)
                                        (with-output-to-string (lambda () (write value)))))))
                       `((value . ,variable))))))))

(define (equalizer? eq)
  (and (= 5 (vector-length eq))
       (every fixnum? (vector->list eq))))

(test-group
 "equalizer?"
 (test #t (equalizer? '#(1 2 3 4 5)))
 (test #f (equalizer? '#(1 2 3 4)))
 (test #f (equalizer? '#(1 2 3 4 a))))

(define-simple-wrapper /v1/player/mute   mute      boolean?)

;; return-value of setter is used as notification payload. setter and
;; notification is only called if the new value is != old value.
;; returns a procedure which, if called with 1 argument, works like a
;; normal cache. if called with 0 arguments, uses getter to fetch the
;; latest value and uses that as the "new value".
;;
;; the returned procedure can be called regularly without arguments to
;; keep send notifications based on changed, polling for changes on
;; whatever getter returns.
(define (with-notification path getter setter value->notification)
  ;; last if wrapped in list so we can distinguish unset values and #f
  ;; from getter.
  (define last #f)
  (define (changed? y) (not (equal? (list y) last)))
  (define (change! y) (set! last (list y)))

  ;; if new-value ≠ old-value, broadcast change and cache new-value
  (define (notify!? new-value)
    (cond ((changed? new-value)
           (send-notification path (value->notification new-value))
           (change! new-value))))

  (lambda (#!optional nval)
    (value->notification
     (cond (nval (notify!? (setter nval)) nval) ;; change by arg
           (else (let ((n (getter)))            ;; change externally
                   (notify!? n)
                   n))))))



(define amixer-eq/notify
  (with-notification "/v1/player/eq"
                     (lambda () (amixer-eq))
                     (lambda (eq) (amixer-eq eq))
                     (lambda (eq) `((value . ,(list->vector eq))))))

;; HACK: use mock volume and eq on macosx
(cond-expand
 ((not arm)
  (define-simple-wrapper /v1/player/volume volume    number?))
 (else
  (define-handler /v1/player/volume
    (lambda ()
;; Note: cmixer can't handle more than 13 characters in the volume setting. Thus we truncate value into 1/10 settings.
      (let ((volume-value (if (current-json)(/ (truncate (* 10. (alist-ref 'value (current-json)))) 10. ) #f )))
        (if (current-json) (amixer-volume volume-value))
        `((value . ,(amixer-volume))))))))

(cond-expand
 ((not arm)
  (define-simple-wrapper /v1/player/eq     equalizer equalizer?))

 (else
  (define-handler /v1/player/eq
    (lambda ()
      (if (current-json)
                    (amixer-eq/notify (vector->list (alist-ref 'value (current-json))))
          (amixer-eq/notify))))))


;; volume watchdog thread. if the volume is modified externally
;; (hardware volume button, for example), we pick it up in cube-server
;; (and also broadcast the changes).

(cond-expand
 ((or arm)
  (begin
    (handle-exceptions e (void) (thread-terminate! amixer-poll-thread))
    (define amixer-poll-thread
      (thread-start!
       (->> (lambda ()
              (amixer-eq/notify))
            (loop/interval 1)
            (loop/exceptions (lambda (e) (pp `(error amixer-poll-thread ,(condition->list e))) #t))
            (loop))))))
 (else '()))
