(import chicken scheme led-matrix)


(define (led-image-key1)
  (begin
    (black)(black)(black)(black)(black)(green)(green)(black)
    (black)(black)(black)(black)(green)(black)(black)(green)
    (black)(black)(black)(black)(green)(black)(black)(green)
    (black)(black)(black)(black)(green)(green)(green)(black)
    (black)(black)(black)(green)(black)(black)(black)(black)
    (black)(black)(green)(green)(black)(black)(black)(black)
    (black)(green)(green)(black)(black)(black)(black)(black)
    (green)(black)(green)(black)(black)(black)(black)(black)
  )
)

(define led-image-key
  (list
    led-image-key1))
