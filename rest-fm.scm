
;; ==================== fm (catalog) notifications ====================
(define *catalog-notify-connections* '())
(define-handler /v1/catalog/notify
  (make-notify-handler (getter-with-setter
                        (lambda () *catalog-notify-connections*)
                        (lambda (new) (set! *catalog-notify-connections* new)))))

;; do a `curl localhost:5060/v1/catalog/notify` and check with
;; (send-notification "foobar" `() *catalog-notify-connections*)


(define (call-when-modified data-thunk thunk #!optional (pred (constantly #t)))
  (define last #f)
  (define (set-last! x) (set! last x))
  (lambda ()
    ;; in case operation is expensive/noisy a predicate can be
    ;; guard the check with a predicate
    (if (pred)
        ;; wrap in list so we can tell the difference between never ran
        ;; and data-thunk returning #f.
        (let ((check (list (data-thunk))))
          (if (and last (equal? check last))
              #t ;; unmodified, keep looping
              (begin (set-last! check)
                     (thunk))))
        ;; keep going if pred is false
        #t)))


(test
 "call-when-modified (rest-fm)"
 "xx"
 (with-input-from-string "aaaaaaaa"
   ;; should be run twice: once the first time, then once again on
   ;; eof.
   (lambda () (->> (call-when-modified read-char (lambda () (display "x")))
              (loop/count 1000)
              (loop)
              (with-output-to-string)))))

;; repl this and your curl localhost:5060/v1/catalog/notify should trigger:
;; (set! *freq* (add1 *freq*))
(define (fm-get-state) `((khz . ,*freq*)))

;; poll-based update mechanism. sends a notification whenever
;; fm-get-state has changed. see loop/interval below for update
;; frequency.
(define fm-thread-iteration
  (call-when-modified
   fm-get-state
   (lambda ()
     (send-notification "/v1/catalog/fm/seek"
                        (fm-get-state)
                        (getter-with-setter (lambda () *catalog-notify-connections*)
                                            (lambda (new) (set! *catalog-notify-connections* new)))))))



(begin
  (handle-exceptions e (void) (thread-terminate! fm-thread))
  (define fm-thread
    (thread-start!
     (make-thread
      (->> (lambda () (fm-thread-iteration))
           (loop/interval 0.5)
           (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                                ,(condition->list e)))
                               #t))
           (loop))
      "fm-thread"))))
