(use test restlib)
(include "amixer.scm")

(import rest)
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


;; HACK: use mock volume and eq on macosx
(cond-expand
 ((not arm)
  (define-simple-wrapper /v1/player/volume volume    number?))
 (else
  (define-handler /v1/player/volume
    (lambda ()
      (let ((vol (if (current-json)
                     (amixer-volume (alist-ref 'value (current-json)))
                     (amixer-volume))))
        `((value . ,vol)))))))


(cond-expand
 ((not arm)
  (define-simple-wrapper /v1/player/eq     equalizer equalizer?))

 (else
  (define-handler /v1/player/eq
    (lambda ()
      (let ((eq (if (current-json)
                    (amixer-eq (vector->list (alist-ref 'value (current-json))))
                    (amixer-eq))))
        `((value . ,(list->vector eq))))))))
