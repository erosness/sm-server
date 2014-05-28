(include "state.scm")


(define state-mutex (make-mutex))
(define (with-state-mutex proc)
  (lambda (#!rest args)
    ((with-mutex-lock state-mutex (lambda () (apply proc args))))))

(begin
  (define state-ref (with-state-mutex state-ref*))
  (define state-ref/default (with-state-mutex state-ref/default*))
  (define state-set! (with-state-mutex state-set!*))
  (define state-merge! (with-state-mutex state-merge!*)))



(define *store-base-url* "/v1/catalog/state/")


(define (make-state key) key)
(define storage (make-state 'wimp))

(state-ref storage) => '((9u0u . "pw"))
(state-set! stroae) => (void)

(define stoarage (make-state 'wimp))
(define key 'wimp)


(define wimp-store (make-store 'wimp))




(define-syntax define-state
  (ir-macro-transformer
   (lambda (x e t)
     (let* ((key (e (cadr x)))
            (path (e (string->symbol (conc *store-base-url* key)))))
       `(define-handler ,path
          (lambda () (if (current-json)
                    (let ((json-request (current-json)))
                      (state-set! (quote ,key) json-request)
                      json-request)
                    (state-ref/default (quote ,key) '()))))))))

(define (make-state-route key)
  (let ((path (string->symbol (conc *store-base-url* key))))
    (define-handler path (lambda () (if (current-json)
                                   (let ((json-request (current-json)))
                                     (state-set! key json-request)
                                     json-request)
                                   (state-ref/default key '()))))))

;; example state
(define-handler /v1/dadw/state  (make-state 'key 'default-variable))
(define-handler /v1/vatalog/state/wimp (make-state 'wimp ))

(make-state-route 'bar)


(define-handler /// (lambda () ...))
(make-state wimp)

(state-ref 'wimp)

(state-set!* 'wimp '((97670550 . "herrowimp")))
(state-merge!* 'wimp '((foo . bar)))


(state-set!* 'foo '((1 . 2)))

(map car (state-ref 'wimp))
