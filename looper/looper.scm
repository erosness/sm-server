;;; helpers for controlling loops. helpful if your're doing things
;;; like:
;;;
;;; - clean exit on threads, use thread-close! and loop/thread
;;;
;;; - retry on socket timeouts (unless thread is closed)
;;;
;;; - close a socket when done (even on error)
(use srfi-18 test clojurian-syntax socket)

;; ==================== timer ====================

;; return a procedure which tells you if seconds has passed since
;; start-timer was evaluated. useful for timeouts.
(define (start-timer seconds)
  (let ((end (+ (current-milliseconds)
                (* seconds 1000))))
    (lambda () (> (current-milliseconds) end))))

(test-group
 "start-timer"
 (let ((timeout? (start-timer 0.05)))
   (test #f (timeout?))
   (thread-sleep! 0.1)
   (test #t (timeout?))))

;; ==================== thread clean close ====================

(define (thread-closed? #!optional (thread (current-thread)))
  (and (not (eq? #!eof (thread-specific thread)))
       #t))

(define (thread-close! thread) (thread-specific-set! thread #!eof))

;; (while (thunk)), returns thunk's result before deciding #f
(define (loop thunk)
  (lambda ()
    (let loop ((last #f))
      (let ((result (thunk)))
       (if result
           (loop result)
           last)))))

(define (loop/when pred thunk)
  (lambda () (and (pred) (thunk))))

;; retry loop as long as thunk and (thread-closed?) don't return #f
(define (loop/thread thunk #!optional (continue? thread-closed?))
  (loop/when continue? thunk))

(define (loop/timeout timeout/sec thunk)
  (let* ((timeout? (start-timer timeout/sec))
         (continue? (lambda () (not (timeout?)))))
    (loop/when continue? thunk)))

;; retry loop as until thunk returns #f, or (handler e) in case of
;; error. your error-handler returns #f to exit the loop.
(define (loop/exceptions handler thunk)
  (lambda () (handle-exceptions e (handler e)
                           (thunk))))

;; retry loop as long as thunk returns #f (catch timeouts and retry)
(define (loop/socket-timeout thunk)
  (lambda ()
    (condition-case
     (thunk)                       ;; <-- retry if thunk
     [(exn i/o net timeout) #t]))) ;; <-- or retry on socket timeout

;; always closes sock after evaling thunk
(define (with-socket sock thunk)
  (lambda ()
    (dynamic-wind void
                  thunk
                  (lambda () (socket-close sock)))))

(test-group
 "loop"

 (define (count-down n)
   (lambda ()
     (set! n (sub1 n))
     (if (>= n 0) n #f)))

 (test #f ((loop (lambda () #f))))
 (test  0 ((loop (count-down 10)))))



(test-group
 "loop/when"
 (test "loop/when continue"  1 ((loop/when (lambda () #t) (lambda () 1))))
 (test "loop/when break" #f ((loop/when (lambda () #f) (lambda () 1)))))

(test-group
 "loop/timeout"
 (let ((to (loop/timeout 0.01 (lambda () 1))))
   (test 1 (to))
   (thread-sleep! 0.02)
   (test #f (to))))

(test-group
 "socket timeout"

 (test "loop/socket-timeout retries"
       #t
       ((loop/socket-timeout
         (lambda () (->> '(exn i/o net timeout)
                    (map make-property-condition)
                    (apply make-composite-condition)
                    (raise))))))

 (test "loop/socket-timeout thunk exits"
       #f ((loop/socket-timeout (lambda () #f)))))

(test-group
 "loop/thread"

 (test "loop/thread continue?"  #f ((loop/thread (lambda () 'ok) (lambda () #f))))
 (test "loop/thread continue?" 'ok ((loop/thread (lambda () 'ok) (lambda () #t))))

 (letrec ((ran? #f)
          (t (make-thread (loop/thread (lambda () (set! ran? #t) 'ok)))))
   (test "thread-closed?" #t (thread-closed? t))
   (thread-close! t)
   (test "thread-closed?" #f (thread-closed? t))
   (test "thread close value" #f (thread-join! (thread-start! t)))
   (test "thunk never ran" #f ran?))

 (test "carry return value" 'ok (thread-join! (thread-start! (loop/thread (lambda () 'ok))))))


(test-group
 "with-socket"
 (let ((closed? #f))
   (fluid-let ((socket-close (lambda (x) (set! closed? #t))))
     (test #t ((with-socket 0 (lambda () #t))))
     (test #t closed?))))
