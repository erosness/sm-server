(use posix miscmacros bitstring)

(define ar 22050)
(define ac 1)

(define (silence sec)
  (with-output-to-string
    (lambda () (repeat (* sec ar ac)
                   (display (integer->char #x80))))))


(define (say text)
  (with-input-from-pipe (conc "espeak \"" text "\" --stdout" " | "
                              "ffmpeg -i - -f u8 -ac 1 - 2>/dev/null")
                        read-string))

(define *pos* 0)
;; current duration in seconds
(define (dur) (/ *pos* ar ac))
(define (seconds) (inexact->exact (floor (dur))))

(define (play str)
  (set! *pos* (+ *pos* (string-length str)))
  (print str))


(define last 0)
(let loop ()
  (let* ((by60 (lambda (x) (inexact->exact (floor (/ x 60)))))
         (now (seconds))
         (minutes (by60 now))
         (seconds (- now (* 60 minutes))))
    (if (not (= last now))
        (begin (set! last now)
               (let ((description
                      (conc seconds " seconds "
                            (if (> minutes 0) (conc  minutes " minute"
                                                     (if (> minutes 1) "s" "")
                                                     " ")
                                ""))))
                 (fprintf (current-error-port) (conc description "\n"))
                 (play (say description))))
        (play (silence 1))))
  (if (< (seconds) 240) (loop)))


