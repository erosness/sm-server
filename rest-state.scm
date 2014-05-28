(import rest concurrent-utils)

(define *store-file* "/tmp/cube-server.store")
(define *store-base-url* "/v1/catalog/state/")

(define *state* (condition-case
                    (with-input-from-file *store-file*
                      (lambda _ (read)))
                  ((exn) '())))

(define (state-ref* key)
  (or (alist-ref key *state*) '()))

(define (state-ref/default* key default)
  (or (alist-ref key *state*) default))

(define (state-set!* key value)
  (set! *state* (alist-update! key value *state*))
  (with-output-to-file *store-file-path* (lambda _
                               (write *state*))))

(define state-mutex (make-mutex))
(define (with-state-mutex proc)
  (lambda (#!rest args)
    ((with-mutex-lock state-mutex (lambda () (apply proc args))))))

(begin
  (define state-ref (with-state-mutex state-ref*))
  (define state-ref/default (with-state-mutex state-ref/default*))
  (define state-set! (with-state-mutex state-set!*)))

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

;; example state
(define-state foo)
