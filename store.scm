(module store (make-store)

(import chicken scheme data-structures)
(use posix srfi-18 extras)

(define *store-location-android* "/data/data")
(define *store-location-dev*     (conc (current-directory) "/data"))

(define (store-location name)
  (let ((filename (conc "/" name "-store.scm")))
    (if (directory? *store-location-android*)
        (conc *store-location-android* filename)
        (let ((loc (conc *store-location-dev* filename)))

          (unless (directory? *store-location-dev*)
              (begin
                (print "creating store directory at " *store-location-dev*)
                (create-directory *store-location-dev*)))

          loc))))


(define (read-store filename default)
  (if (file-exists? filename)
      (with-input-from-file filename read)
      default))

(define (write-store filename value)
  (with-output-to-file filename
    (lambda _ (write value))))

;; name can be filename (string) or symbol (-> filename through
;; store-location). should be thread-safe.
(define (make-store name #!optional default)

  (define mutex (make-mutex (conc name "-store")))
  (define filename (cond ((string? name) name)
                         (else (store-location name))))
  (lambda (#!rest val)
    (dynamic-wind
      (lambda () (mutex-lock! mutex))
      (lambda ()
        (if (not (null? val))
            (let ((val (car val)))
              (write-store filename val)
              val)
            (read-store filename default)))
      (lambda () (mutex-unlock! mutex)))))

(use test)
(test-group
 "store"
 (test "/data/data/wimp-store.scm"
       (fluid-let ((directory? (lambda _ #t)))
         (store-location "wimp")))

 (let* ((filename (conc "store-test." (+ 1000000 (random 1000000))))
        (store (make-store filename 100)))
   (test #f (file-exists? filename))
   (test 100 (store)) ;; <-- use default value
   (test 101 (store 101))
   (test filename (file-exists? filename))
   (test "make new store with same filename (should read file contents)"
         101 ((make-store filename -1)))

   (delete-file filename)))

)
