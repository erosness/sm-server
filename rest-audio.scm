(use test)
(import rest)

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
       (every number? (vector->list eq))))

(test-group
 "equalizer?"
 (test #t (equalizer? '#(1 2 3 4 5)))
 (test #f (equalizer? '#(1 2 3 4)))
 (test #f (equalizer? '#(1 2 3 4 a))))

(define-simple-wrapper /mute   mute      boolean?)
(define-simple-wrapper /volume volume    number?)
(define-simple-wrapper /eq     equalizer equalizer?)
