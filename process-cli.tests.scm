(use test)

(test-group
 "spawn*"
 ;; this is tricky to test. we use thread-sleep! which probably isn't a
 ;; good idea but it works for now.
 (let ((line #f))
   (receive (pip pop pid) (spawn* "cat" '())

     (thread-start! (lambda () (set! line (read-line pip))))
     ;; give room for read-thread
     (thread-sleep! 0.05)
     ;; and check that it doesn't block our thread
     (test #f line)
     (display "hello world\n" pop)
     (flush-output pop)
     (thread-sleep! 0.05)
     (test "hello world" line)
     
     (close-input-port pip)
     (close-output-port pop)
     (define done #f)
     (thread-start! (lambda () (process-wait pid #f) (set! done #t)))
     (thread-sleep! 0.05)
     (test #t done)))
 

 (receive (pip pop pid)
     (spawn* "sleep" '("0")) ;; <-- exits immediately
   (thread-sleep! 0.1)
   ;; we want to error here with "broken pipe"
   (test-error "check that sigpipe interrupt is disabled"
               (display "hello world\n" pop))
   (flush-output pop)
   (process-wait pid))
 )


(test-group
 "process-cli"

 (letrec ((exited? #f)
          (cli (process-cli "sleep" '("0.1") (lambda () (set! exited? #t)))))
   (test "on-exit not called" #f exited?)
   (thread-sleep! 0.2)
   (test "on-exit called" #t exited?)
   (cli #:quit))



 (let ((cli (process-cli "cat" '() (lambda () (void))))
       (threads 100))
   (test
    "process-cli thread-safety"
    (make-list threads #t)
    (map
     thread-join!
     (list-tabulate threads
                    (lambda (ix)
                      (-> (lambda ()
                            (thread-yield!) ;; <-- for some action
                            (= ix (string->number (cli (number->string ix)))))
                        (thread-start!))))))
   (cli #:quit))


 (let ((cli (process-cli "cat" '() (lambda () #f))))
   (test "a b" (cli "a b"))
   (test "ef" (cli ef:))
   (test-error (cli "a\nb"))
   (cli #:quit))
 )
