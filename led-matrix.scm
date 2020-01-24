;; Returns access procedures to GPIO, alternatively an emulated version.

(module led-matrix
  (red green blue dimblue black white yellow mangenta cyan
   animate-thread display-thread)

(import chicken scheme extras srfi-18 clojurian-syntax data-structures)
(use looper)

(define (red) (write-byte #x00) (write-byte #xF0))
(define (green) (write-byte #x00) (write-byte #x0F))
(define (blue) (write-byte #x1f) (write-byte #x00))
(define (dimblue) (write-byte #x0f) (write-byte #x00))
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

(define display-thread #f)

(define (animate-thread led-list #!optional (time 0.2) (repeat #t))
  (if display-thread (thread-terminate! display-thread))
  (set! display-thread
    (thread-start!
      (->>
        (lambda () (if (animate-led led-list time) repeat #f))
        (loop/interval time)
        (loop)
        ((flip make-thread) "Animate-thread")))))

(define (image-off)
  (begin
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
  )
)
;; Initial set to off if ARM
(animate-thread (list image-off))

)
