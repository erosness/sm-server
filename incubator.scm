(module incubator *

(import chicken scheme data-structures srfi-1)

(use test srfi-13 srfi-18)


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

;; Do 'keys' in 'xs' and 'ys' have the same value?
;; Does not support nested alists
(define (alist-equal-keys? xs ys keys)
  (let loop ((keys keys))
    (if (null? keys) #t
        (and (let ((xval (alist-ref (car keys) xs))
                   (yval (alist-ref (car keys) ys)))
               (and xval yval (equal? xval yval)))
             (loop (cdr keys))))))

(let ((alist1 '((foo . 1) (bar . 2) (baz . 3) (laps . 4)))
      (alist2 '((foo . 1) (bar . 2) (baz . 4) (zoo . "12")))
      (alist3 '((bar . 2))))
 (test-group
  "alist-equal-keys?"
  (test "both has key equal" #t (alist-equal-keys? alist1 alist2 '(foo bar)))
  (test "both has key not equal" #f (alist-equal-keys? alist1 alist2 '(foo bar baz)))
  (test "both has not key" #f (alist-equal-keys? alist1 alist2 '(laps zoo)))
  (test "empty keyset" #t (alist-equal-keys? alist1 alist2 '()))))

;; Measure time based on wallclock time
(define (wallclock-time thunk)
  (define clock current-milliseconds)
  (let ((before (clock))
        (res (thunk))
        (after  (clock)))
    (print "Time: " (- after before))
    res))

;;; Atom
;; -------------------------------------------------------
;; like make-parameter except:
;; - argument is a procedure instead of the new value
;; - same value across all threads
;; - still thread-safe
;; - optional non-procedural argument sets it to the new value
(define (make-atom initial)
  (define mutex (make-mutex))
  (define value initial)
  (lambda a
    (mutex-lock! mutex)
    (if (pair? a)
        (handle-exceptions e
          ;; catch, unlock and throw back - better than dynamic-wind,
          ;; apparently, because new threads don't have default
          ;; error-handlers which means the `after` thunk of
          ;; dynamic-wind doesn't get called.
          (begin (mutex-unlock! mutex)
                 (raise e))
          (set! value (let ((proc (car a)))
                        (if (procedure? proc)
                            (proc value) ;; atomic update
                            proc)))))    ;; atmoc overwrite new value
    (let ((v value))
      (mutex-unlock! mutex)
      v)))

(test-group
 "make-atom"
 (define a (make-atom 0))
 (for-each thread-join!
           (map (lambda (_) (thread-start! (lambda () (thread-yield!) (a add1))))
                (iota 100)))
 (test 100 (a))


 ;; error recovery
 (test-error "error in atomic update proc"
             (a (lambda (hundred) (error "i will recover"))))
 (a add1)
 (test "post-error updates work" 101 (a))

 ;; non-procedure updates
 (a 200)
 (test "set new value (no proc)" 200 (a))
 (test "immediate return value" 300 (a 300)))
;; -------------------------------------------------------
;; Atom end

)
