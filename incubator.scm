(module incubator *

(import chicken scheme data-structures srfi-1)

(use test)

;; Merges several alists into one. If a key is present in more than
;; one alist the latter will be used.
(define (alist-merge #!rest alists)
  (fold (lambda (alist result)
          (if alist
              (fold (lambda (item res)
                      (alist-update (car item) (cdr item) res)) result alist)
              result))
        '() alists))

(test-group
 "alist-merge"
 (test "basic merge" '((foo . 2)) (alist-merge '((foo . 1)) '((foo . 2))))
 (test "use key from latter list" '((foo . 1) (bar . 2))
       (alist-merge '((foo . 2) (bar . 2)) '((foo . 1))))
 (test "works with empty list" '() (alist-merge '()))
 ;; Note that ordering matters when comparing for equality
 (test "introduce new keys" '((baz . 3) (foo . 1) (bar . 2))
       (alist-merge '((baz . 2) (foo . 1)) '((bar . 2) (foo . 99)) '((baz . 3) (foo . 1))))
 (test "alist can be false" '((foo . 1) (bar . 2))
       (alist-merge '((foo . 1)) #f '((bar . 2)))))
)
