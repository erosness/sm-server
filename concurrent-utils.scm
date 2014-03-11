(module concurrent-utils (with-mutex-lock)

(import chicken scheme ports)
(use srfi-18 test)

;; simple mutex wrapper (don't nest it, it will error with deadlock!)
(define (with-mutex-lock mutex procedure)
  (lambda args
    (dynamic-wind
      (lambda () (mutex-lock! mutex))
      (lambda () (apply procedure args))
      (lambda () (mutex-unlock! mutex)))))

(test-group
 "with-mutex-lock"
 (test
  "locks work"
  ;; one thread does its business at a time
  "aaaaabbbbbccccc"
  (call-with-output-string
   (lambda (port)
     (let* ((prn (lambda ()
                   (for-each (lambda (x)
                               (thread-yield!)
                               (display (thread-name (current-thread)) port))
                             '(1 2 3 4 5))))
            (mx (make-mutex))
            (thunk (with-mutex-lock mx prn))
            (threads (map (lambda (label) (make-thread thunk label))
                          '("a" "b" "c"))))
       (for-each thread-start! threads)
       (for-each thread-join!  threads)))))

 (let ((h (with-mutex-lock (make-mutex) (lambda a a))))
   (test "arguments are passed properly"
         '(1 2 3)
         (h 1 2 3))))
)
