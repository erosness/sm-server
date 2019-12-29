;; Returns access procedures to GPIO, alternatively an emulated version.

(module led-matrix *

(import chicken scheme extras srfi-18 looper clojurian-syntax data-structures)

(define (red) (write-byte #x00) (write-byte #xF0))
(define (green) (write-byte #x00) (write-byte #x0F))
(define (blue) (write-byte #x1f) (write-byte #x00))
(define (black) (write-byte #x00) (write-byte #x00))
(define (white) (write-byte #x1f) (write-byte #xFF))
(define (yellow) (write-byte #x00) (write-byte #xFF))
(define (mangenta) (write-byte #x1F) (write-byte #xF0))
(define (cyan) (write-byte #x1F) (write-byte #x0F))


(define (display-led-image led-image)
  (with-output-to-file "/dev/fb1" (lambda () (led-image))))

(define (display-list image-list #!optional (time 0.2) (count 0))
  (if (pair? image-list)
    (begin
      (display-led-image (car image-list))
      (thread-sleep! (seconds->time (+ time (time->seconds(current-time)))))
      (if (pair? image-list)
        (+ 1 (display-list (cdr image-list) time count))))
    count))

(define (animate-led led-list #!optional (time 0.2))
  (call/cc
    (lambda (k)
      (with-exception-handler (lambda (x) (k #f))
                              (lambda ()
                                (display-list led-list time))))))


(define (animate-thread led-list #!optional (time 0.2) (repeat #t))
  (thread-start!
    (->>
      (lambda () (print "loop:" (animate-led led-list time)) #f)
      (loop/interval 0.5)
      (loop)
      ((flip make-thread) "Animate-thread"))))

)
