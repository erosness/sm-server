(use test restlib)
(include "amixer.scm")

(import rest)
(import notify)
(import amixer)

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
(define (with-notification path getter setter)
  ;; last if wrapped in list so we can distinguish unset values and #f
  ;; from getter.
  (define last #f)
  (define (update x) (set! last (list x)))
  (define (changed? y) (not (equal? (list y) last)))
  (lambda (#!optional (nval (getter)))
    (if (changed? nval)
        (begin
          (send-notification path (setter nval))
          (set! last (list nval))))
    nval))

;; calling /notify versions without arguments will query amixer for
;; current volume and notify if changed. calling it with argument,
;; will set that value using amixer, cache it for later comparison,
;; and send notify messages to everybody.
(define amixer-volume/notify
  (with-notification "/v1/player/volume" amixer-volume
                     (lambda (volume)
                       `((value . ,(amixer-volume volume))))))

(define amixer-eq/notify
  (with-notification "/v1/player/eq" amixer-eq
                     (lambda (eqlst)
                       `((value . ,(list->vector (amixer-eq eqlst)))))))

;; HACK: use mock volume and eq on macosx
(cond-expand
 ((not arm)
  (define-simple-wrapper /v1/player/volume volume    number?))
 (else
  (define-handler /v1/player/volume
    (lambda ()
      (let ((vol (if (current-json)
                     (amixer-volume/notify (alist-ref 'value (current-json)))
                     (amixer-volume/notify))))
        `((value . ,vol)))))))


(cond-expand
 ((not arm)
  (define-simple-wrapper /v1/player/eq     equalizer equalizer?))

 (else
  (define-handler /v1/player/eq
    (lambda ()
      (let ((eq (if (current-json)
                    (amixer-eq/notify (vector->list (alist-ref 'value (current-json))))
                    (amixer-eq/notify))))
        `((value . ,(list->vector eq))))))))
