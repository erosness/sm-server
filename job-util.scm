;;; Small util functions to restart threads/jobs when they die.
(use srfi-18)


;; run proc in an infinite loop, printing all exceptions but never
;; giving up.
(define (job-auto-respawn proc #!optional (respawn-delay 1))
  (assert (procedure? proc))
  (let loop ()
    (handle-exceptions exn
      (print "respawning failed job: " proc "(" (print-error-message exn) ")")
      (proc))
    (thread-sleep! respawn-delay)
    (loop)))

