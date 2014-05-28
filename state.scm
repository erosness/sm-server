(import rest concurrent-utils incubator)

(define *store-file* "/data/cube-server-storage.scm")
(define *store-file-android* (conc "/data" *store-file*))
(define *store-file-dev*     (conc (current-directory) *store-file* ))

(define *store-file-path* (if (regular-file? *store-file-android*)
                              *store-file-android*
                              *store-file-dev*))

(define *store-base-url* "/v1/catalog/state/")

(define *state* (condition-case
                    (with-input-from-file *store-file-path*
                      (lambda _ (let ((m (read)))
                             (if (list? m) m '()))))
                  ((exn) '())))

(define (state-ref* key)
  (or (alist-ref key *state*) '()))

(define (state-ref/default* key default)
  (or (alist-ref key *state*) default))

(define (state-set!* key value)
  (set! *state* (alist-update! key value *state*))
  (with-output-to-file *store-file-path* (lambda _
                               (write *state*))))

(define (state-merge!* key value)
  (let ((current-value (state-ref* key)))
    (state-set!* 'wimp (alist-merge current-value value))
    `(status . ok)))

(define state-mutex (make-mutex))
(define (with-state-mutex proc)
  (lambda (#!rest args)
    ((with-mutex-lock state-mutex (lambda () (apply proc args))))))

(begin
  (define state-ref (with-state-mutex state-ref*))
  (define state-ref/default (with-state-mutex state-ref/default*))
  (define state-set! (with-state-mutex state-set!*))
  (define state-merge! (with-state-mutex state-merge!*)))

