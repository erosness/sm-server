(module store (make-store)

(import chicken scheme data-structures)
(use posix srfi-18)

(define *store-location-android* "/data/data")
(define *store-location-dev*     (conc (current-directory) "/data"))

(define (store-location name)
  (let ((filename (conc "/" name "-store.scm")))
    (if (directory? *store-location-android*)
        (conc *store-location-android* filename)
        (conc *store-location-dev* filename))))

(define (read-store name)
  (condition-case
      (with-input-from-file (store-location name)
        (lambda _ (let ((m (read))) (if (list? m) m '()))))
    ((exn) '())))

(define (write-store name value)
  (with-output-to-file (store-location name)
    (lambda _ (write value)))
  `((status . "ok")))

(define (make-store name)
  (define mutex (make-mutex (conc name "-store")))
  (lambda (#!rest val)
    (dynamic-wind
      (lambda () (mutex-lock! mutex))
      (lambda ()
        (if (not (null? val))
            (let ((val (car val)))
              (write-store name val))
            (read-store name)))
      (lambda () (mutex-unlock! mutex)))))
)