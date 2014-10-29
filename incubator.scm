(module incubator *

(import chicken scheme data-structures srfi-1)

(use test srfi-13)


(define (alist=? a b)
  ;; this should work but is really slow (converting numbers into
  ;; strings, then sorting, really? we're almost becoming javascript)
  (define (key>? a b) (string>= (->string (car a)) (->string (car b))))

  (and (list? a) (list? b)
       (equal? (sort a key>?)
               (sort b key>?))))

(define (equal/alist? a b)
  (or (equal? a b) (alist=? a b)))

(test-group
 "equal/alist?"
 (test #t (equal/alist? 1 1))
 (test #f (equal/alist? 1 0))
 (test #t (equal/alist? "a" "a"))


 (test #t (equal/alist? `((a . 1) (b . 2))
                        `((b . 2) (a . 1))))

 (test #f (equal/alist? `((a . 1) (b . 1))
                        `((a . 1) (b . 2))))

 (test #t (equal/alist? `((1 . #t))
                        `((1 . #t))))

 (test "we don't handle duplicates yet"
       ;; these are actually equal, no?
       #f (equal/alist? `((a . 1))
                        `((a . 1) (a . 1)))))

;; like alist-ref, but we're comparing keys as string and doing it
;; recursively. also doen't vomit if state if #f. kinda like clojure,
;; I guess! you'll love this, Peder.
(define (get-in state . keys)
  (let loop ((state state)
             (keys keys))
    (if (pair? keys)
        (and state
             (loop (alist-ref (car keys) state equal?) (cdr keys)))
        state)))

(test-group
 "get-in"
 (test #f (get-in '() 'a))
 (test #f (get-in '() 'a 'b))

 (test 1 (get-in '((a . 1)) 'a))
 (test #f (get-in '((a . ((b . 0)))) 'a 'x))

 (test 2 (get-in '((a . ((b . 2)))) 'a 'b)))



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

;; Measure time based on wallclock time
(define (wallclock-time proc)
  (define clock current-milliseconds)
  (let ((before (clock))
        (res (proc))
        (after  (clock)))
    (print "Time: " (- after before))
    res))
