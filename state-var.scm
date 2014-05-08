(import concurrent-utils)
(use test)

(define-record-type state (%make-state value mutex listeners)
  state?
  (value state-value %state-value-set!)
  (mutex state-mutex)
  (listeners %state-listeners %state-listeners-set!))

(define-record-printer (state s port)
  (display (conc "#<state " (state-value s)
                 " listeners: " (length (%state-listeners s))
                 ">") port))

(define (make-state #!optional (value #f) (listeners '()))
  (%make-state value (make-mutex) listeners))

(define (state-change* s newval)
  (let ((oldval (state-value s))
        (listeners (%state-listeners s)))
    (if (not (equal? oldval newval))
        (begin (%state-value-set! s newval)
               (for-each
                (lambda (proc)
                  (proc oldval newval))
                listeners)))))

(define (state-add-listener* s listener)
  (let ((listeners (%state-listeners s)))
    (%state-listeners-set! s (cons listener listeners))))

(define (with-state-mutex proc)
  (lambda (state . args)
    ((with-mutex-lock (state-mutex state) (lambda () (apply proc (cons state args)))))))

(define state-change (with-state-mutex state-change*))
(define state-add-listener (with-state-mutex state-add-listener*))

(test-group
 "state-var"
 (test "basic set/get" 5 (state-value (make-state 5)))
 (test "value defaults to #f" #f (state-value (make-state)))
 (test "change state value" 6 (let ((s (make-state)))
                                (state-change s 6)
                                (state-value s)))
 
 ;; you can add a listener to a state-var that runs on each state-change
 (test "state change listener" 7 (let ((switch 99)
                                       (s (make-state "hello")))
                                   (state-add-listener s (lambda (oldval newval)
                                                           (set! switch newval)))
                                   (state-change s 7)
                                   switch)))
