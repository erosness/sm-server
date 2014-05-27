(import rest concurrent-utils)

(define *store-file* "/tmp/cube-server.store")
(define *store-base-url* "/v1/catalog/state/")

(define *state* (condition-case
                    (with-input-from-file *store-file* (lambda _
                                                  (let ((m (read)))
                                                    (alist->hash-table m))))
                  ((exn) (make-hash-table))))

(define (state-ref* key)
  (hash-table-ref *state* key))

(define (state-ref/default* key default)
  (hash-table-ref/default *state* key default))

(define (state-set!* key value)
  (hash-table-set! *state* key value)
  (with-output-to-file *store-file* (lambda _
                               (write (hash-table->alist *state*)))))

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
